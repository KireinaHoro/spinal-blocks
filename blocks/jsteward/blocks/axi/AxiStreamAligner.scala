package jsteward.blocks.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis._
import spinal.lib.fsm._

/**
 * For a stream that has a gap at the beginning, after process by [[AxiStreamExtractHeader]], shift the stream such
 * that the gap is closed and the stream is aligned.
 * Does not handle "holes" (TKEEP[x] == 0) in the middle of the stream.
 *
 * @param axisConfig AXI-Stream config for input and output
 */
case class AxiStreamAligner(axisConfig: Axi4StreamConfig) extends Component {
  val io = new Bundle {
    val input = slave(Axi4Stream(axisConfig))
    val output = master(Axi4Stream(axisConfig))
  }

  assert(!axisConfig.useStrb, "TSTRB not supported")
  assert(!axisConfig.useUser, "TUSER not supported")
  assert(!axisConfig.useId, "TID not supported")
  assert(axisConfig.useLast, "must enable TLAST or we don't know packet boundary")
  assert(axisConfig.useKeep, "must enable TKEEP or there are no gaps to close")

  // example operation for 64b, shift = 5 (* 8):
  // MSB ............... LSB
  //  1  1  1  0  0  0  0  0
  // AA AA AA xx xx xx xx xx
  // CC CC CC BB BB BB BB BB

  // we do not allow beats with keep all zero, therefore only 6 bits of keep
  assert(!io.input.valid || io.input.keep =/= B(0))

  // only valid during first beat
  val toShiftFirstBeat = CountTrailingZeroes(io.input.keep).resize(log2Up(axisConfig.dataWidth) - 1)
  // saved for all following beats
  val toShiftSaved = Reg(toShiftFirstBeat)
  // select which to use
  val toShift = CombInit(toShiftFirstBeat)

  val shiftedBeat = io.output.payload.clone
  // MSB ............... LSB
  // xx xx xx xx xx AA AA AA
  // BB BB BB BB BB CC CC CC
  // FIXME: is this efficient enough, or do we need our own impl with two shifting?
  shiftedBeat.data := io.input.data.rotateRight(toShift * 8)
  shiftedBeat.keep := io.input.keep.rotateRight(toShift)

  // head fragment to put into staging registers (AA AA AA)
  val headDataMask = io.input.data.getAllTrue |>> (toShift * 8)
  val headKeepMask = io.input.keep.getAllTrue |>> toShift
  val headBeat = io.output.payload.clone
  headBeat.data := shiftedBeat.data & headDataMask
  headBeat.keep := shiftedBeat.keep & headKeepMask
  headBeat.last := False

  // tail fragment to put into staging registers (BB BB BB BB BB)
  val tailDataMask = io.input.data.getAllTrue |<< ((axisConfig.dataWidth - toShift) * 8)
  val tailKeepMask = io.input.keep.getAllTrue |<< (axisConfig.dataWidth - toShift)
  val tailBeat = io.output.payload.clone
  tailBeat.data := shiftedBeat.data & tailDataMask
  tailBeat.keep := shiftedBeat.keep & tailKeepMask
  tailBeat.last := False

  // staging area for assembling output
  val stagingBeats = Vec.fill(2)(Reg(io.output.payload))
  val nextStaging = Reg(UInt(1 bit)) init 0
  val nextFullBeat = io.output.payload.clone
  nextFullBeat.data := stagingBeats(nextStaging).data & headDataMask | tailBeat.data
  nextFullBeat.keep := stagingBeats(nextStaging).keep & headKeepMask | tailBeat.keep
  nextFullBeat.last := False
  // MSB ............... LSB
  // before:
  // .. .. .. .. .. AA AA AA <-- nextStaging
  // .. .. .. .. .. .. .. ..
  // after:
  // BB BB BB BB BB AA AA AA
  // .. .. .. .. .. CC CC CC <-- nextStaging

  io.input.setBlocked()
  io.output.setIdle()

  // handle last -- two cases:
  // - fewer bytes than fragment -- output staged beat as last
  // - more bytes than fragment -- need one more beat as last

  val fsm = new StateMachine {
    val idle: State = new State with EntryPoint {
      whenIsActive {
        io.input.freeRun()
        io.output.setIdle()
        toShift := toShiftFirstBeat
        when(io.input.valid) {
          toShiftSaved := toShift
          // save stage beat but don't output yet since we don't have a full beat
          stagingBeats(nextStaging) := headBeat
          io.input.ready := True

          when(io.input.last) {
            // stream only has one beat -- output directly
            headBeat.last := True
            io.output.payload := headBeat
            io.output.valid := True
            when (!io.output.ready) {
              goto(waitLast)
            }
          } otherwise {
            goto(captureFragment)
          }
        }
      }
      onExit {
        toShift := toShiftSaved
      }
    }
    val captureFragment: State = new State {
      whenIsActive {
        when(io.input.valid) {
          stagingBeats(nextStaging) := nextFullBeat
          stagingBeats(1 - nextStaging) := headBeat
          io.input.ready := True
          // we now have a full beat, output directly and also save
          io.output.payload := nextFullBeat
          when(io.input.last) {
            when(headBeat.keep === B(0)) {
              // we don't have tail to output, this is the last beat to downstream
              nextFullBeat.last := True
            } otherwise {
              // another head beat left to output
              headBeat.last := True
            }
          }
          io.output.valid := True

          when(nextFullBeat.last) {
            when(!io.output.ready) {
              goto(waitLast)
            } otherwise {
              goto(idle)
            }
          } otherwise {
            when(!io.output.ready) {
              // downstream stalled
              goto(waitFragment)
            } otherwise {
              nextStaging := 1 - nextStaging
            }
          }
        }
      }
    }
    val waitFragment: State = new State {
      whenIsActive {
        // output registered value
        io.output.payload := stagingBeats(nextStaging)
        when(io.output.ready) {
          nextStaging := 1 - nextStaging
          goto(captureFragment)
        }
      }
    }
    val waitLast: State = new State {
      whenIsActive {
        io.output.payload := stagingBeats(nextStaging)
        when(io.output.ready) {
          goto(idle)
        }
      }
    }
  }
}
