package jsteward.blocks.misc

import spinal.core._
import spinal.lib.misc.plugin._
import spinal.lib.fsm._
import spinal.lib._

import scala.language.postfixOps

/** Record transactions on multiple `Flow[T]` and store them in a memory
 * buffer.  A trigger input streams the content of the buffer out to a dump
 * port.  Each trace event is tagged with the index of the input port and
 * timestamp when the event happened.
 */
case class TraceBuffer[T <: Data](
                                   payloadType: HardType[T],
                                   numInputs: Int,
                                   numSlots: Int,
                                   burstFifoSize: Int = 16,
                                   timestampWidth: Int = 64,
                                 ) extends Component {
  case class CapturedEvent() extends Bundle {
    val event = payloadType()
    val src = UInt(log2Up(numInputs) bits)
    val ts = UInt(timestampWidth bits)
  }

  val traceIn = Vec(slave(Flow(payloadType)), numInputs)
  val traceOut = out(CapturedEvent())
  val dump = in(Bool())
  val sampleLost = out(RegInit(False))
  traceOut.clearAll()

  val cycleCount = CounterFreeRun(timestampWidth bits)
  val storage = Mem(CapturedEvent(), numSlots)

  val dumpAddr, captureAddr = Counter(numSlots)
  val readoutFsm = new StateMachine {
    val idle: State = new State with EntryPoint {
      whenIsActive {
        dumpAddr.clearAll()
        when (dump.rise(False)) {
          goto(readout)
        }
      }
    }
    val readout = new State {
      whenIsActive {
        traceOut := storage(dumpAddr)
        dumpAddr.increment()
        when (dumpAddr.willOverflow) {
          goto(idle)
          sampleLost.clear()
        }
      }
    }
  }

  val catPorts = Flow(traceIn)
  catPorts.valid := traceIn.map(_.valid).orR
  catPorts.payload zip traceIn foreach { case (cp, p) => cp := p }

  // use a small FIFO to resolve conflicts, and also keep a flag when we overflow
  val overflow = Bool()
  val bufferedPorts = catPorts.toStream(overflow, burstFifoSize, burstFifoSize)
  bufferedPorts.ready := False
  sampleLost.setWhen(overflow)

  val savedPorts = bufferedPorts.toFlowFire.toReg()
  savedPorts.foreach { sp => sp.valid init False }
  val savedTs = RegNextWhen(cycleCount.value, bufferedPorts.fire)
  val nextPort = OHToUInt(OHMasking.first(savedPorts.map(_.valid)))

  // create one write port for both init and capture
  val writeData = CombInit(savedPorts(nextPort).map { p =>
    val ret = CapturedEvent()
    ret.event := p
    ret.src := nextPort
    ret.ts := savedTs
    ret
  })
  val writeEn = CombInit(False)
  storage.write(captureAddr, writeData, writeEn)

  val captureFsm = new StateMachine {
    val init: State = new State with EntryPoint {
      whenIsActive {
        writeEn := True
        writeData.clearAll()
        captureAddr.increment()
        when (captureAddr.willOverflow) {
          goto(idle)
        }
      }
    }
    val idle: State = new State {
      whenIsActive {
        bufferedPorts.ready := True
        when (bufferedPorts.valid) {
          goto(captureEvent)
        }
      }
    }
    val captureEvent = new State {
      whenIsActive {
        writeEn := True
        captureAddr.increment()
        savedPorts(nextPort).valid := False
        when (CountOne(savedPorts.map(_.valid)) === 1) {
          // that was the last one, we're done
          goto(idle)
        }
      }
    }
  }
}
