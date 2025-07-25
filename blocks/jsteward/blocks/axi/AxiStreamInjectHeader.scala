package jsteward.blocks.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis._
import spinal.lib.fsm._

import scala.language.postfixOps

/**
  * Inject [[headerLen]] bytes into an [[Axi4Stream]].  Assumes that any NULL bytes are only present at the beginning
  * of the stream, and that no completely NULL beats may be present.
  *
  * This module is intended to be chained.
  *
  * @param axisConfig AXI-Stream config for input and output
  * @param headerLen length of header, in bytes
  */
case class AxiStreamInjectHeader(axisConfig: Axi4StreamConfig, headerLen: Int) extends Component {
  val headerWidth = headerLen * 8
  val beatWidth = axisConfig.dataWidth * 8
  val headerTailLen = headerLen % axisConfig.dataWidth

  val io = new Bundle {
    val input = slave(Axi4Stream(axisConfig))
    val output = master(Axi4Stream(axisConfig))
    val header = slave(Stream(Bits(headerWidth bits)))
  }

  assert(!axisConfig.useStrb, "TSTRB not supported")
  assert(axisConfig.useKeep, "must enable TKEEP or we can't strip part of beat")
  assert(axisConfig.useLast, "must enable TLAST or we don't know packet boundary")
  assert(axisConfig.useKeep || headerTailLen == 0, "either enable TKEEP, or the header should be a multiple of TDATA")

  io.output.assertPersistence()

  assert(!io.output.valid || io.output.keep =/= 0, "output should not have all NULL beats")
  assert(!io.input.valid || io.input.keep =/= 0, "input should not have all NULL beats")

  val headerLenWidth = log2Up(headerLen) + 1
  val segmentLenWidth = log2Up(axisConfig.dataWidth) + 1

  val outHeaderOff, outHeaderLen = Reg(UInt(headerLenWidth bits)) init 0
  val firstBeatGap = Reg(UInt(segmentLenWidth bits)) init 0

  val headerBuffered = Reg(Bits(headerWidth bits))
  val firstBeatBuffered = Reg(Bits(beatWidth bits))
  val firstKeepBuffered = Reg(Bits(axisConfig.dataWidth bits))
  val firstLastBuffered = Reg(Bool())

  val headerFrag, lastHeaderFrag = Bits(beatWidth bits)
  val headerFragMask, lastHeaderFragMask = Bits(axisConfig.dataWidth bits)
  val lastHeaderToShift = UInt(segmentLenWidth bits)

  headerFragMask := ((U(1, axisConfig.dataWidth bits) |<< outHeaderLen) - 1).asBits
  headerFrag := headerBuffered.resize(beatWidth)

  // header buffer always shifted against LSB
  // TODO: break into multiple states
  lastHeaderToShift := firstBeatGap - outHeaderLen
  lastHeaderFragMask := headerFragMask |<< lastHeaderToShift
  lastHeaderFrag := headerFrag |<< (lastHeaderToShift * 8)

  def selectWithByteMask(data: Bits, byteEnable: Bits): Bits = new Composite(data, "masked") {
    val ret = data.clone()
    data.subdivideIn(8 bits) zip
      ret.subdivideIn(8 bits) zip
      byteEnable.asBools foreach { case ((d, r), e) =>
      r := e ? d | 0
    }
  }.ret

  val maskedInput = selectWithByteMask(io.input.data, io.input.keep)

  io.input.setBlocked()
  io.output.setIdle()
  io.header.setBlocked()

  val fsm = new StateMachine {
    val idle: State = new State with EntryPoint {
      whenIsActive {
        firstBeatBuffered := 0
        firstKeepBuffered := 0
        firstLastBuffered := False
        headerBuffered := 0

        io.header.ready := True
        when (io.header.valid) {
          headerBuffered := io.header.payload
          goto(captureFirstBeat)
        }
      }
    }
    val captureFirstBeat = new State {
      whenIsActive {
        io.input.ready := True
        when (io.input.valid) {
          // calculate and store shifts
          val dataBeatGap = CountTrailingZeroes(io.input.keep).resize(segmentLenWidth)
          firstBeatGap := dataBeatGap
          outHeaderOff := 0

          firstBeatBuffered := maskedInput
          firstKeepBuffered := io.input.keep
          firstLastBuffered := io.input.last

          when (dataBeatGap <= headerLen) {
            // gap smaller than header
            outHeaderLen := (headerTailLen - dataBeatGap).resized
            goto(outputHeaderBeats)
          } otherwise {
            // gap bigger than entire header
            outHeaderLen := headerLen
            goto(outputFirstDataBeat)
          }
        }
      }
    }
    val outputFirstDataBeat = new State {
      whenIsActive {
        assert(outHeaderOff + outHeaderLen === headerLen, "did not consume the entire header")
        assert(firstBeatGap >= outHeaderLen, "too much header left, does not fit inside gap of first beat")

        // we need to check this since we are OR-ing with first beat data
        assert(selectWithByteMask(lastHeaderFrag, lastHeaderFragMask) === lastHeaderFrag, "garbage left over on MSB")

        io.output.data := firstBeatBuffered | lastHeaderFrag
        io.output.keep := firstKeepBuffered | lastHeaderFragMask
        io.output.last := firstLastBuffered
        io.output.valid := True

        when (io.output.ready) {
          when (firstLastBuffered) {
            goto(idle)
          } otherwise {
            goto(passthrough)
          }
        }
      }
    }
    val outputHeaderBeats = new State {
      whenIsActive {
        io.output.data := headerFrag
        io.output.keep := headerFragMask
        io.output.last := False
        io.output.valid := True

        when (io.output.ready) {
          val newOff = outHeaderOff + outHeaderLen
          outHeaderOff := newOff
          headerBuffered := headerBuffered >> (outHeaderLen * 8)

          val hdrLeft = headerLen - newOff
          assert((hdrLeft - firstBeatGap) % axisConfig.dataWidth === 0, "length calculation error")
          when (hdrLeft === firstBeatGap) {
            outHeaderLen := hdrLeft
            goto(outputFirstDataBeat)
          } otherwise {
            // if we are not writing the first data beat (other arm of this when),
            // we must be outputing a full header beat now
            // header size must be at least one full beat
            assert(axisConfig.dataWidth <= outHeaderLen.resize(segmentLenWidth), "ran out of header")
            outHeaderLen := U(axisConfig.dataWidth).resized
            // just sent a whole header beat, stay here
          }
        }
      }
    }
    val passthrough = new State {
      whenIsActive {
        io.output << io.input
        when (io.output.lastFire) {
          goto(idle)
        }
      }
    }
  }
}

object Test extends App {
  SpinalVerilog(AxiStreamInjectHeader(Axi4StreamConfig(dataWidth = 64, useLast = true, useKeep = true), 15))
}