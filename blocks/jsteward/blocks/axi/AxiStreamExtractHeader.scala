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
    val input = slave(Axi4Stream(axisConfig))
    val output = master(Axi4Stream(axisConfig))
    val header = master(Stream(Bits(outputLen * 8 bits)))
  }

  val beatCaptured = Reg(io.output.payload)
  val headerCaptured = Reg(io.header.payload)

  assert(!axisConfig.useStrb, "TSTRB not supported")
  assert(axisConfig.useLast, "must enable TLAST or we don't know packet boundary")
  assert(axisConfig.useKeep || outputLen % axisConfig.dataWidth == 0, "either enable TKEEP, or the header should be a multiple of TDATA")

  io.input.setBlocked()
  io.output.setIdle()
  io.header.setIdle()
  io.header.assertPersistence()

  val outputLenWidth = log2Up(outputLen)
  val segmentLenWidth = log2Up(axisConfig.dataWidth) + 1
  val headerRemaining = Reg(UInt(outputLenWidth bits)) init outputLen
  val beatSlice = CombInit(B(0, outputLen * 8 bits))
  val consumeLen = CombInit(U(0, outputLenWidth bits))
  val segmentLen, segmentOffset = CombInit(U(0, segmentLenWidth bits))
  val dataMask = if (axisConfig.useKeep) CombInit(B(0, axisConfig.dataWidth * 8 bits)) else null
  val keepMask = if (axisConfig.useKeep) CombInit(B(0, axisConfig.dataWidth bits)) else null

  def consumeSegment: Unit = new Area {
    if (!axisConfig.useKeep) {
      segmentOffset := 0
      segmentLen := axisConfig.dataWidth
      beatSlice := beatCaptured.data
    } else {
      segmentOffset := CountTrailingZeroes(beatCaptured.keep).resized // no AXIS beat should have TKEEP all low
      segmentLen := CountTrailingZeroes(~(beatCaptured.keep >> segmentOffset)).resize(segmentLenWidth)

      dataMask := ((U(1) << (consumeLen * 8)) - 1).asBits.resized
      keepMask := ((U(1) << consumeLen) - 1).asBits.resized
      beatSlice := ((beatCaptured.data >> segmentOffset) & dataMask).resized
      // mask off segment we just consumed
      beatCaptured.keep := beatCaptured.keep & ~(keepMask << segmentOffset).resize(axisConfig.dataWidth)
    }

    when (headerRemaining <= segmentLen) {
      consumeLen := headerRemaining
    } otherwise {
      consumeLen := segmentLen.resized
    }

    // store partial header
    headerCaptured := headerCaptured | (beatSlice << (outputLen - headerRemaining)).resized
    headerRemaining := headerRemaining - consumeLen

    // when we got the entire header AND there are still segments left in the beat:
    // pass through beat with TKEEP cleared for the bytes consumed
    // (we don't pass through a beat with all zero TKEEP)
    when (headerRemaining === 0) {
      io.header.valid := True
      io.header.payload := headerCaptured
      when (io.header.ready) {
        fsm.goto(fsm.writeEarlyBody)
      }
    } otherwise {
      when (beatCaptured.keep === 0) {
        // beat is depleted but header not complete yet, need new one
        io.input.ready := True
        fsm.goto(fsm.captureBeat)
        // TODO: check if TLAST asserted -- unexpected end of packet
        //       report error in CSR
      }
    }
  } setName "consumeSegment"

  val fsm = new StateMachine {
    val idle: State = new State with EntryPoint {
      whenIsActive {
        io.input.freeRun()
        io.output.setIdle()
        io.header.setIdle()
        headerCaptured.clearAll()
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
    val writeEarlyBody: State = new State {
      def checkLastExit = when (beatCaptured.last) {
          // keep == 0 AND last beat: we got a header only packet
          // TODO: report to CSR
          goto(idle)
        } otherwise {
          // not last beat: more to follow
          goto(writeBody)
        }
      whenIsActive {
        when (beatCaptured.keep =/= 0) {
          // beat still has remaining segments, pass through
          io.output.valid := True
          io.output.payload := beatCaptured
          when (io.output.ready) {
            checkLastExit
          }
        } otherwise {
          checkLastExit
        }
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
