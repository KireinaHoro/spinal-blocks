package jsteward.blocks.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis.Axi4Stream.Axi4StreamBundle
import spinal.lib.bus.amba4.axis._
import spinal.lib.fsm._

// extract the first outputLen bytes from Axi4Stream
// output stream without header
case class AxiStreamExtractHeader(axisConfig: Axi4StreamConfig, outputLen: Int) extends Component {
  val io = new Bundle {
    val input = in(Axi4Stream(axisConfig))
    val output = out(Axi4Stream(axisConfig))
    val header = out(Stream(Bits(outputLen * 8 bits))).setOutputAsReg()
  }

  val beatCaptured = Reg(io.output.payload)

  assert(!axisConfig.useStrb, "TSTRB not supported")
  assert(axisConfig.useKeep || outputLen % axisConfig.dataWidth == 0, "either enable TKEEP, or the header should be a multiple of TDATA")

  io.input.setBlocked()
  io.output.setIdle()
  io.header.setIdle()

  val lenWidth = log2Up(outputLen)
  val headerRemaining = Reg(UInt(lenWidth bits)) init outputLen

  def consumeSegment: Unit = {
    val beatSlice = io.header.payload.clone
    val segmentLen, segmentOffset = UInt(lenWidth bits)

    if (!axisConfig.useKeep) {
      // consume entire beat
      beatSlice := beatCaptured.data
      segmentOffset := 0
      if (outputLen > axisConfig.dataWidth) {
        segmentLen := axisConfig.dataWidth
      } else {
        segmentLen := outputLen
      }
    } else {
      segmentOffset := CountTrailingZeroes(beatCaptured.keep)
      segmentLen := CountTrailingZeroes(~(beatCaptured.keep >> segmentOffset))
      val dataMask = ((U("1") << segmentLen) - 1).asBits
      beatSlice := (beatCaptured.data >> segmentOffset) & dataMask
      // mask off segment we just consumed
      beatCaptured.keep := beatCaptured.keep & ~(dataMask << segmentOffset)
    }

    // store partial header
    io.header.payload := io.header.payload | (beatSlice.resized << (outputLen - headerRemaining))
    when (headerRemaining <= segmentLen) {
      headerRemaining := 0
    } otherwise {
      headerRemaining := headerRemaining - segmentLen
    }

    // when we got the entire header AND there are still segments left in the beat:
    // pass through beat with TKEEP cleared for the bytes consumed
    // (we don't pass through a beat with all zero TKEEP)
    when (headerRemaining === 0) {
      io.header.valid := True
      when (io.header.ready) {
        io.header.valid := False
        when (beatCaptured.keep =/= 0) {
          // beat still has remaining segments, pass through
          io.output.valid := True
          io.output.payload := beatCaptured
          io.input.ready := io.output.ready
          when (io.output.ready) {
            fsm.goto(fsm.writeBody)
          }
        } otherwise {
          fsm.goto(fsm.writeBody)
        }
      }
    } otherwise {
      when (beatCaptured.keep === 0) {
        // beat is depleted but header not complete yet, need new one
        io.input.ready := True
        fsm.goto(fsm.captureBeat)
      }
    }
  }

  val fsm = new StateMachine {
    val idle: State = new State with EntryPoint {
      whenIsActive {
        io.input.freeRun()
        io.output.setIdle()
        io.header.setIdle()
        headerRemaining := outputLen
        when (io.input.valid) {
          beatCaptured := io.input.payload
          goto(writeHeader)
        }
      }
    }
    val captureBeat: State = new State {
      whenIsActive {
        when (io.input.valid) {
          beatCaptured := io.input.payload
          goto(writeHeader)
        }
      }
    }
    val writeHeader: State = new State {
      whenIsActive {
        consumeSegment
      }
    }
    val writeBody: State = new State {
      whenIsActive {
        // pass through rest of the stream
        io.output << io.input
        when (io.output.lastFire) {
          goto(idle)
        }
      }
    }
  }
}
