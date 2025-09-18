package jsteward.blocks.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis._
import spinal.lib.fsm._

/** Drop/take a frame based on trigger.  Uses a FIFO to keep track of pending drop / pass requests.
  * in case many triggers arrive at the same time.
  */
object FilterAction extends SpinalEnum {
  val drop, pass = newElement()
}

case class AxiStreamFilter(
                            axisConfig: Axi4StreamConfig,
                            maxPendingActions: Int = 32,
                          ) extends Component {
  val io = new Bundle {
    val input = slave(Axi4Stream(axisConfig))
    val output = master(Axi4Stream(axisConfig))
    val action = slave(Flow(FilterAction()))
  }

  assert(axisConfig.useLast, "must enable TLAST or we don't know packet boundary")

  val actionFifo = StreamFifoLowLatency(FilterAction(), maxPendingActions)
  val pushOverflow = Bool()
  actionFifo.io.push << io.action.toStream(pushOverflow)
  assert(!pushOverflow, "pending action overflow")

  io.input.setBlocked()
  io.output.setIdle()
  actionFifo.io.pop.ready := False

  val fsm = new StateMachine {
    val idle: State = new State with EntryPoint {
      whenIsActive {
        when (io.input.valid) {
          assert(actionFifo.io.pop.valid, "no action enqueued for frame")
          when (actionFifo.io.pop.payload === FilterAction.drop) {
            goto(dropFrame)
          } otherwise {
            goto(passFrame)
          }
        }
      }
    }
    val dropFrame: State = new State {
      whenIsActive {
        io.input.ready := True
        when (io.input.lastFire) {
          actionFifo.io.pop.ready := True
          goto(idle)
        }
      }
    }
    val passFrame: State = new State {
      whenIsActive {
        io.output << io.input
        when (io.input.lastFire) {
          actionFifo.io.pop.ready := True
          goto(idle)
        }
      }
    }
  }
}
