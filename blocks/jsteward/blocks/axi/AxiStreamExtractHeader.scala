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

  io.header.payload.setAsReg() init 0
  io.header.valid := False

  val headerLenWidth = log2Up(maxHeaderLen) + 1
  val segmentLenWidth = log2Up(axisConfig.dataWidth) + 1
  val headerRemaining = Reg(UInt(headerLenWidth bits)) init maxHeaderLen

  // how much did we consume (store into header)?
  val consumeLen = CombInit(U(0, headerLenWidth bits))

  val headerRemainingNext = headerRemaining - consumeLen

  // by assumption, all following bytes are non-null
  // unless the stream is only one beat and still has trailing gap
  val segmentOffset = CountTrailingZeroes(io.input.keep).resize(segmentLenWidth)
  // find first non-null byte
  // since we skip all NULL beats in idle, keep =/= 0
  val segmentLen = (axisConfig.dataWidth - segmentOffset - CountLeadingZeroes(io.input.keep)).resize(segmentLenWidth)

  val dataMask = ((U(1) << (consumeLen * 8)) - 1).asBits.resized
  val keepMask = ((U(1) << consumeLen) - 1).asBits.resized

  // fragment of header to store into headerCaptured
  val headerFragment = ((io.input.data >> (segmentOffset * 8)) & dataMask).resize(maxHeaderLen * 8)
  val headerFragmentShifted = headerFragment |<< ((maxHeaderLen - headerRemaining) * 8).resized

  // keep with the captured fragment removed
  io.output.keep := io.input.keep & ~(keepMask << segmentOffset).resize(axisConfig.dataWidth)
  io.output.data := io.input.data
  io.output.last := io.input.last
  io.output.valid := io.input.valid
  io.input.ready := io.output.ready

  def captureHeaderFragmentAndOutput(): Unit = {
    // store fragment
    when (io.input.fire && io.input.keep =/= B(0)) {
      io.header.payload := io.header.payload | headerFragmentShifted
      headerRemaining := headerRemainingNext

      when(headerRemainingNext === 0) {
        // we have a full header
        when(io.output.last) {
          when(io.output.keep === B(0)) {
            inc(_.headerOnly)
          }
          fsm.goto(fsm.emitHeaderLast)
        } otherwise {
          fsm.goto(fsm.emitHeader)
        }
      } elsewhen (io.input.valid && io.input.last) {
        // no full header yet, but already last beat
        when(headerRemainingNext <= maxHeaderLen - minHeaderLen) {
          // minimum header achieved, emit partial
          fsm.goto(fsm.emitHeaderLast)
          inc(_.partialHeader)
        } otherwise {
          // minimum header not achieved, drop header
          fsm.goto(fsm.idle)
          inc(_.incompleteHeader)
        }
      } otherwise {
        // no full header and not last beat
        fsm.goto(fsm.captureHeader)
      }
    }
  }

  def clearHeaderBuf(): Unit = {
    io.header.payload.clearAll()
    headerRemaining := maxHeaderLen
  }

  val fsm = new StateMachine {
    val idle: State = new State with EntryPoint {
      whenIsActive {
        clearHeaderBuf()
        captureHeaderFragmentAndOutput()
      }
    }
    val emitHeader: State = new State {
      whenIsActive {
        // halt streams during header output
        io.input.ready := False
        io.output.valid := False

        io.header.valid := True
        when (io.header.ready) {
          clearHeaderBuf()
          goto(passthrough)
        }
      }
    }
    val emitHeaderLast: State = new State {
      whenIsActive {
        // halt streams during header output
        io.input.ready := False
        io.output.valid := False

        io.header.valid := True
        when (io.header.ready) {
          clearHeaderBuf()
          goto(idle)
        }
      }
    }
    val captureHeader: State = new State {
      whenIsActive {
        captureHeaderFragmentAndOutput()
      }
    }
    val passthrough: State = new State {
      whenIsActive {
        when (io.output.lastFire) {
          goto(idle)
        }
      }
    }
  }

  when (fsm.isActive(fsm.passthrough)) {
    consumeLen := 0
  } elsewhen (headerRemaining <= segmentLen) {
    consumeLen := headerRemaining
  } otherwise {
    consumeLen := segmentLen.resized
  }
}
