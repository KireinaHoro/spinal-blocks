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
import jsteward.blocks.eci.EciCmdDefs
import spinal.core.sim.waitUntil
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
 * @groupname cpu APIs for CPU simulation clients (i.e. testbench).  These will be called from a thread simulation context
 *                so it's ok to block
 * @groupname dcs APIs for [[DcsAppMaster]].  These will be called from a callback of StreamMonitor so we should not
 *                block!
 */
class DcsStateMachineSim(id: String, loadStore: ClLoadStore) {
  private var data: List[Byte] = List.fill(EciCmdDefs.ECI_CL_SIZE_BYTES)(0xff.toByte)
  private var _state: EciClState = Invalid
  private var _locked = false

  private def log(msg: String) = println(s"DcsStateMachineSim $id: $msg")

  def dumpState(): Unit = {
    log(s"[${_state.name}] (${if (locked) "L" else "."}) ${if (state != Invalid) data.bytesToHex else ""}")
  }
  def state: EciClState = _state

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
    val oldData = data
    data = mutator(data)
    assert(data.length == EciCmdDefs.ECI_CL_SIZE_BYTES)
    log(s"data change: ${oldData.bytesToHex} -> ${data.bytesToHex}")
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
    assert(!locked, s"trying to lock $id that's already locked")
    _locked = true
    log("locked")
  }

  /**
   * Unlock this cacheline.
   * @group dcs
   */
  private[sim] def unlock(): Unit = {
    assert(locked)
    _locked = false
    log("unlocked")
  }
  private[sim] def locked: Boolean = _locked

  private var inRefill = false
  private def doRefill() = {
    inRefill = true
    data = loadStore.load
    inRefill = false
  }

  private var inFlush = false
  private def doFlush() = {
    inFlush = true
    loadStore.store(data)
    inFlush = false
  }
  private[sim] def memInProgress: Boolean = inRefill || inFlush

  private def transition(newState: EciClState)(body: EciClState => Unit) = {
    if (state != newState) {
      log(s"${state.name} -> ${newState.name}")
      // prevent upgrade if is lock
      if (state < newState) waitUntil(!locked)
      body(state)
      _state = newState
      dumpState()
    }
  }

  /** Change state to modified.
   * @group dcs
   */
  private[sim] def toModified() = {
    transition(Modified) {
      case Invalid => doRefill()
      case _ =>
    }
  }

  /** Change state to exclusive.
   * @group dcs
   */
  private[sim] def toExclusive() = {
    transition(Exclusive) {
      case Invalid => doRefill()
      case Modified => doFlush()
      case _ =>
    }
  }

  /** Change state to shared.
   * @group dcs
   */
  private[sim] def toShared() = {
    transition(Shared) {
      case Invalid => doRefill()
      case Modified => doFlush()
      case _ =>
    }
  }

  /** Change state to invalid.
   * @group dcs
   */
  private[sim] def toInvalid() = {
    transition(Invalid) {
      case Modified => doFlush()
      case _ =>
    }
  }
}