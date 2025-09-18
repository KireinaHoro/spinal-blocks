package jsteward.blocks.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis._
import spinal.lib.fsm._

/** Drop/take a frame based on trigger.  Uses a counter to keep track of how many more frames to take / drop,
  * in case many triggers arrive at the same time.
  */
case class AxiStreamDropFrame(
                               axisConfig: Axi4StreamConfig,
                               triggerDoDrop: Boolean,
                               maxPendingTriggers: Int = 32,
                             ) extends Component {
  val io = new Bundle {
    val input = slave(Axi4Stream(axisConfig))
    val output = master(Axi4Stream(axisConfig))
    val trigger = in Bool()
  }

  assert(axisConfig.useLast, "must enable TLAST or we don't know packet boundary")

  val pending = CounterUpDown(maxPendingTriggers)
  assert(!pending.willOverflow, s"pending counter overflow")
  assert(!pending.willUnderflow, s"pending counter underflow")
  when (io.trigger) {
    pending.increment()
  }

  io.input.setBlocked()
  io.output.setIdle()

  val fsm = new StateMachine {
    val idle: State = new State with EntryPoint {
      whenIsActive {
        when (io.input.valid) {
          if (triggerDoDrop) {
            when (pending > 0) {
              goto(dropFrame)
            } otherwise {
              goto(passFrame)
            }
          } else {
            when (pending > 0) {
              goto(passFrame)
            } otherwise {
              goto(dropFrame)
            }
          }
        }
      }
    }
    val dropFrame: State = new State {
      whenIsActive {
        io.input.ready := True
        when (io.input.lastFire) {
          if (triggerDoDrop) {
            pending.decrement()
          }
          goto(idle)
        }
      }
    }
    val passFrame: State = new State {
      whenIsActive {
        io.output << io.input
        when (io.input.lastFire) {
          if (!triggerDoDrop) {
            pending.decrement()
          }
          goto(idle)
        }
      }
    }
  }
}
