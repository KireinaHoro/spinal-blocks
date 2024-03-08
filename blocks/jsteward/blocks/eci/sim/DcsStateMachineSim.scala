package jsteward.blocks.eci.sim

object EciClStates extends Enumeration {
  type EciClState = Value

  val Invalid, Shared, Exclusive, Modified = Value

  implicit class EciClStateValue(v: Value) {
    def name: String = v match {
      case Invalid => "I"
      case Shared => "S"
      case Exclusive => "E"
      case Modified => "M"
    }
  }
}
import EciClStates._
import spinal.core.sim.SimMutex
import spinal.lib.BytesRicher

trait ClLoadStore {
  def load: List[Byte]
  def store(d: List[Byte]): Unit
}

/**
 * Reflects the state of one cacheline on the CPU (__remote__ from the viewpoint of the FPGA).  The module maps actions
 * from the CPU (`read`, `modify`, etc.) to actions on the DCS memory interface.
 * @param id address of the cacheline (only for dumping state)
 * @param loadStore load and store functions to refill and flush cacheline
 */
class DcsStateMachineSim(id: String, loadStore: ClLoadStore) {
  private var data: List[Byte] = List.fill(128)(0xff.toByte)
  private var state: EciClState = Invalid
  private val m = SimMutex()

  private def log(msg: String) = println(s"$id: $msg")

  def dumpState(): Unit = {
    log(s"[${state.name}] (${if (locked) "L" else "."}) ${data.bytesToHex}")
  }

  // TODO: more accessor patterns from CPU
  def read: List[Byte] = {
    toShared()
    data
  }
  def modify(mutator: List[Byte] => List[Byte]): Unit = {
    toModified()
    data = mutator(data)
  }
  def invalidate(): Unit = {
    toInvalid()
  }

  def lock(): Unit = {
    assert(Seq(Invalid, Shared).contains(state))
    m.lock()
  }
  def unlock(): Unit = {
    m.unlock()
  }
  def locked: Boolean = m.locked

  private def transition(newState: EciClState) = {
    log(s"${state.name} -> ${newState.name}")
    state = newState
  }

  def toModified() = {
    m.await()
    state match {
      case Invalid => data = loadStore.load
      case _ =>
    }
    transition(Modified)
  }

  def toExclusive() = {
    m.await()
    state match {
      case Invalid => data = loadStore.load
      case Modified => loadStore.store(data)
      case _ =>
    }
    transition(Exclusive)
  }

  def toShared() = {
    m.await()
    state match {
      case Invalid => data = loadStore.load
      case Modified => loadStore.store(data)
      case _ =>
    }
    transition(Shared)
  }

  def toInvalid() = {
    m.await()
    state match {
      case Modified => loadStore.store(data)
      case _ =>
    }
    transition(Invalid)
  }
}