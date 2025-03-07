package jsteward.blocks.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis.Axi4Stream.Axi4StreamBundle
import spinal.lib.bus.amba4.axis._
import spinal.lib.fsm._

/**
 * Extract (up to) the first [[maxHeaderLen]] bytes from an [[Axi4Stream]]; these bytes will be stripped from the output
 * stream.  Assumes that any NULL bytes are only present at the beginning of the stream.  This module is intended to be chained.
 * To enable partial header output, set [[minHeaderLen]] smaller than [[maxHeaderLen]].  This feature can be used to
 * handle IP headers, for example.
 *
 * The module guarantees that no output beats will happen before the header is sent.
 *
 * @param axisConfig   AXI-Stream config for input and output
 * @param maxHeaderLen maximum length of header, in bytes
 * @param minHeaderLen minimum length of header, in bytes; default to [[maxHeaderLen]] (no partial headers)
 */
case class AxiStreamExtractHeader(axisConfig: Axi4StreamConfig, maxHeaderLen: Int)(minHeaderLen: Int = maxHeaderLen) extends Component {
  assert(minHeaderLen <= maxHeaderLen, s"minimum header length $minHeaderLen bigger than maximum header length $maxHeaderLen")

  val io = new Bundle {
    val input = slave(Axi4Stream(axisConfig))
    val output = master(Axi4Stream(axisConfig))
    val header = master(Stream(Bits(maxHeaderLen * 8 bits)))
    val statistics = out(new Bundle {
      val headerOnly = Reg(UInt(64 bits))
      val partialHeader = Reg(UInt(64 bits))
      val incompleteHeader = Reg(UInt(64 bits))
    })
  }

  def inc(f: io.statistics.type => UInt) = {
    f(io.statistics) := f(io.statistics) + 1
  }

  io.statistics.flatten.foreach(_ init U(0))

  assert(!axisConfig.useStrb, "TSTRB not supported")
  assert(axisConfig.useKeep, "must enable TKEEP or we can't strip part of beat")
  assert(axisConfig.useLast, "must enable TLAST or we don't know packet boundary")
  assert(axisConfig.useKeep || maxHeaderLen % axisConfig.dataWidth == 0, "either enable TKEEP, or the header should be a multiple of TDATA")

  io.output.assertPersistence()
  io.header.assertPersistence()

  // we shouldn't output completely NULL beats
  assert(!io.output.valid || io.output.keep =/= 0, "output should not have NULL beats")
  // we also do not handle completely NULL beats
  assert(!io.input.valid || io.input.keep =/= 0, "input should have NULL beats")

  io.header.payload.setAsReg() init 0
  io.header.valid := False

  val headerLenWidth = log2Up(maxHeaderLen) + 1
  val segmentLenWidth = log2Up(axisConfig.dataWidth) + 1
  val headerRemaining = Reg(UInt(headerLenWidth bits)) init maxHeaderLen

  // by assumption, all following bytes are non-null
  // unless the stream is only one beat and still has trailing gap
  val segmentOffset = CountTrailingZeroes(io.input.keep).resize(segmentLenWidth)
  // find first non-null byte
  // since we skip all NULL beats in idle, keep =/= 0
  val segmentLen = (axisConfig.dataWidth - segmentOffset - CountLeadingZeroes(io.input.keep)).resize(segmentLenWidth)

  // how much did we consume (store into header)?
  val consumeLen = (headerRemaining <= segmentLen) ? headerRemaining | segmentLen.resized
  val headerRemainingNext = headerRemaining - consumeLen

  // for masking out KEEP in first beat of output
  val keepMask = ((U(1) << consumeLen) - 1).asBits.resized
  val keepMaskSaved = Reg(keepMask) init 0

  // get fragment of shifted header to OR into header.payload
  val dataMask = ((U(1) << (consumeLen * 8)) - 1).asBits.resized
  val headerFragment = ((io.input.data >> (segmentOffset * 8)) & dataMask).resize(maxHeaderLen * 8)
  // XXX: potential optimization:
  // - MSB-align header.payload
  // - for a new segment, OR into LSB and then barrel-shift to right
  val headerFragmentShifted = headerFragment |<< ((maxHeaderLen - headerRemaining) * 8).resized
  val headerFragmentSaved = RegNextWhen(headerFragmentShifted, io.input.valid) init 0

  // keep with the captured fragment removed
  io.output.keep := io.input.keep & ~(keepMaskSaved << segmentOffset).resize(axisConfig.dataWidth)
  io.output.data := io.input.data
  io.output.last := io.input.last
  io.output.valid := io.input.valid
  io.input.ready := io.output.ready

  def clearHeaderBuf(): Unit = {
    io.header.payload.clearAll()
    headerRemaining := maxHeaderLen
  }

  def haltAll(): Unit = {
    io.input.ready := False
    io.output.valid := False
  }

  val fsm = new StateMachine {
    def captureValidBeat(): Unit = {
      when (io.input.valid) {
        headerRemaining := headerRemainingNext

        // capture fragment of header saved in headerFragmentSaved
        when (headerRemainingNext === 0) {
          // we have a full header, save first beat mask and emit header
          keepMaskSaved := keepMask
          when (consumeLen === segmentLen && io.input.last) {
            // we ate up the last byte
            inc(_.headerOnly)
          }
          goto(emitHeader)
        } elsewhen (io.input.last) {
          // no full header yet, but already last beat
          // must have consumed all bytes in beat
          // let passthrough to consume the input beat
          keepMaskSaved := keepMask
          when (headerRemainingNext <= maxHeaderLen - minHeaderLen) {
            // minimum header achieved, emit partial
            goto(emitHeader)
            inc(_.partialHeader)
          } otherwise {
            // minimum header not achieved, drop header
            goto(idle)
            io.input.ready := True
            inc(_.incompleteHeader)
          }
        } otherwise {
          // no full header yet, more beats to come
          io.input.ready := True
          when (!io.input.valid) {
            goto(waitBeat)
          } otherwise {
            goto(captureHeader)
          }
        }
      }
    }

    val idle: State = new State with EntryPoint {
      whenIsActive {
        haltAll()
        clearHeaderBuf()
        captureValidBeat()
      }
    }
    val captureHeader: State = new State {
      whenIsActive {
        haltAll()
        captureValidBeat()
        io.header.payload := io.header.payload | headerFragmentSaved
      }
    }
    val waitBeat: State = new State {
      whenIsActive {
        captureValidBeat()
      }
    }
    val emitHeader: State = new State {
      whenIsActive {
        haltAll()
        io.header.payload := io.header.payload | headerFragmentSaved
        goto(emitHeader2)
      }
    }
    val emitHeader2: State = new State {
      whenIsActive {
        haltAll()
        io.header.valid := True
        when (io.header.ready) {
          clearHeaderBuf()
          goto(passthrough)
        }
      }
    }
    val passthrough: State = new State {
      whenIsActive {
        when (io.input.valid && io.output.keep === 0) {
          io.output.valid := False
        }
        when (io.input.fire) {
          keepMaskSaved.clearAll()
        }
        when (io.input.lastFire) {
          goto(idle)
        }
      }
    }
  }
}
