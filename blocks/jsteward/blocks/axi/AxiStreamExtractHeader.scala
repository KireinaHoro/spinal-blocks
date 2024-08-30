package jsteward.blocks.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis.Axi4Stream.Axi4StreamBundle
import spinal.lib.bus.amba4.axis._
import spinal.lib.fsm._

/**
 * Extract (up to) the first [[maxHeaderLen]] bytes from an [[Axi4Stream]]; these bytes will be stripped from the output
 * stream.  Emits NULL bytes but also consumes them properly.  This module is intended to be chained.
 * To enable partial header output, set [[minHeaderLen]] smaller than [[maxHeaderLen]].
 *
 * TODO: allow header length target to be changed at runtime to decode stuff like IP options
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

  val beatCaptured = Reg(io.output.payload)
  val headerCaptured = Reg(io.header.payload)

  assert(!axisConfig.useStrb, "TSTRB not supported")
  assert(axisConfig.useLast, "must enable TLAST or we don't know packet boundary")
  assert(axisConfig.useKeep || maxHeaderLen % axisConfig.dataWidth == 0, "either enable TKEEP, or the header should be a multiple of TDATA")

  io.input.setBlocked()
  io.output.setIdle()
  io.header.setIdle()
  io.header.assertPersistence()

  val headerLenWidth = log2Up(maxHeaderLen) + 1
  val segmentLenWidth = log2Up(axisConfig.dataWidth) + 1
  val headerRemaining = Reg(UInt(headerLenWidth bits)) init maxHeaderLen
  val beatSlice = CombInit(B(0, maxHeaderLen * 8 bits))
  val consumeLen = CombInit(U(0, headerLenWidth bits))
  val segmentLen, segmentOffset = CombInit(U(0, segmentLenWidth bits))
  val dataMask = if (axisConfig.useKeep) CombInit(B(0, axisConfig.dataWidth * 8 bits)) else null
  val keepMask = if (axisConfig.useKeep) CombInit(B(0, axisConfig.dataWidth bits)) else null

  val fsm = new StateMachine {
    val idle: State = new State with EntryPoint {
      whenIsActive {
        io.input.freeRun()
        io.output.setIdle()
        io.header.setIdle()
        headerCaptured.clearAll()
        headerRemaining := maxHeaderLen
        when (io.input.valid) {
          beatCaptured := io.input.payload
          goto(writeHeader)
        }
      }
    }
    val captureBeat: State = new State {
      whenIsActive {
        io.input.ready := True
        when (io.input.valid) {
          beatCaptured := io.input.payload
          goto(writeHeader)
        }
      }
    }
    val writeHeader: State = new State {
      whenIsActive {
        !axisConfig.useKeep generate {
          segmentOffset := 0
          segmentLen := axisConfig.dataWidth
          beatSlice := beatCaptured.data
        }
        axisConfig.useKeep generate {
          segmentOffset := CountTrailingZeroes(beatCaptured.keep).resized // no AXIS beat should have TKEEP all low
          segmentLen := CountTrailingZeroes(~(beatCaptured.keep >> segmentOffset)).resize(segmentLenWidth)

          dataMask := ((U(1) << (consumeLen * 8)) - 1).asBits.resized
          keepMask := ((U(1) << consumeLen) - 1).asBits.resized
          beatSlice := ((beatCaptured.data >> (segmentOffset * 8)) & dataMask).resized
          // mask off segment we just consumed
          beatCaptured.keep := beatCaptured.keep & ~(keepMask << segmentOffset).resize(axisConfig.dataWidth)
        }

        when (headerRemaining <= segmentLen) {
          consumeLen := headerRemaining
        } otherwise {
          consumeLen := segmentLen.resized
        }

        // store partial header
        headerCaptured := headerCaptured | (beatSlice << ((maxHeaderLen - headerRemaining) * 8)).resized
        headerRemaining := headerRemaining - consumeLen

        // write header
        def emitHeader(nextState: State) = {
          io.header.valid := True
          io.header.payload := headerCaptured
          when (io.header.ready) {
            goto(nextState)
          }
        }

        when (headerRemaining === 0) {
          // got the required header and a captured beat;
          // beat might have TKEEP all zero but that's fine
          emitHeader(writeEarlyBody)
        } otherwise {
          when (beatCaptured.keep === 0) {
            // beat is depleted but header not complete yet, need new one
            when (beatCaptured.last) {
              // header not completely filled
              when (headerRemaining <= maxHeaderLen - minHeaderLen) {
                // minimum header achieved -- emit partial header
                when (io.header.fire) { inc(_.partialHeader) }
                emitHeader(idle)
              } otherwise {
                // minimum header not achieved -- drop header
                inc(_.incompleteHeader)
                goto(idle)
              }
            } otherwise {
              goto(captureBeat)
            }
          }
        }
      }
    }
    val writeEarlyBody: State = new State {
      whenIsActive {
        // we allow an empty beat (TKEEP == 0) to simplify state transition logic
        io.output.valid := True
        io.output.payload := beatCaptured
        when (io.output.ready) {
          when (beatCaptured.last) {
            when (beatCaptured.keep === 0) {
              // keep == 0 AND last beat: we got a header only packet
              inc(_.headerOnly)
            }
            goto(idle)
          } otherwise {
            // not last beat: more to follow
            goto(writeBody)
          }
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
