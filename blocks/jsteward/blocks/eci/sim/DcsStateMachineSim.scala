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
 * @groupname cpu APIs for CPU simulation clients (i.e. testbench)
 * @groupname dcs APIs for [[DcsAppMaster]]
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
  /**
   * Read from this cacheline.
   * @group cpu
   */
  def read: List[Byte] = {
    toShared()
    data
  }

  /**
   * Map the cacheline to new contents.
   * @group cpu
   */
  def modify(mutator: List[Byte] => List[Byte]): Unit = {
    toModified()
    data = mutator(data)
  }

  /**
   * Invalidate the current cacheline and write-back if dirty.
   * @group cpu
   */
  def invalidate(): Unit = {
    toInvalid()
  }

  /**
   * Lock this cacheline so that it doesn't respond to load/store and state transition requests.
   * @group dcs
   */
  private[sim] def lock(): Unit = {
    assert(Seq(Invalid, Shared).contains(state))
    m.lock()
  }

  /**
   * Unlock this cacheline.
   * @group dcs
   */
  private[sim] def unlock(): Unit = {
    m.unlock()
  }
  private[sim] def locked: Boolean = m.locked

  private def transition(newState: EciClState) = {
    log(s"${state.name} -> ${newState.name}")
    state = newState
  }

  /** Change state to modified.
   * @group dcs
   */
  private[sim] def toModified() = {
    m.await()
    state match {
      case Invalid => data = loadStore.load
      case _ =>
    }
    transition(Modified)
  }

  /** Change state to exclusive.
   * @group dcs
   */
  private[sim] def toExclusive() = {
    m.await()
    state match {
      case Invalid => data = loadStore.load
      case Modified => loadStore.store(data)
      case _ =>
    }
    transition(Exclusive)
  }

  /** Change state to shared.
   * @group dcs
   */
  private[sim] def toShared() = {
    m.await()
    state match {
      case Invalid => data = loadStore.load
      case Modified => loadStore.store(data)
      case _ =>
    }
    transition(Shared)
  }

  /** Change state to invalid.
   * @group dcs
   */
  private[sim] def toInvalid() = {
    m.await()
    state match {
      case Modified => loadStore.store(data)
      case _ =>
    }
    transition(Invalid)
  }
}