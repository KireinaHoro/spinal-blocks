package jsteward.blocks.eci.sim

import jsteward.blocks.eci.{DcsInterface, EciCmdDefs, LclChannel}
import jsteward.blocks.eci.sim.BigIntRicher
import spinal.core.{ClockDomain, IntToBuilder, roundUp}
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba4.axi.sim.Axi4Master
import spinal.lib.sim._

import scala.collection.mutable
import EciCmdDefs.ECI_CL_SIZE_BYTES

/**
 * Simulation master for an Enzian app using the Directory Controller Slice (DCS) interface; refer to CCKit for more
 * details.  The master mimics memory transfers from the DCS'es and keeps a internal cache-line state machine (in
 * [[DcsStateMachineSim]]).
 * @param dcsEven DCS interface for even cachelines (in aliased addresses)
 * @param dcsOdd DCS interface for odd cachelines (in aliased addresses)
 * @param clockDomain the clock domain of ECI
 */
case class DcsAppMaster(dcsEven: DcsInterface, dcsOdd: DcsInterface, clockDomain: ClockDomain) {
  // index is unaliased address
  val clMap = mutable.HashMap[BigInt, DcsStateMachineSim]()
  val dcsOddAxiMaster = Axi4Master(dcsOdd.axi, clockDomain, "dcsOdd")
  val dcsEvenAxiMaster = Axi4Master(dcsEven.axi, clockDomain, "dcsEven")

  dcsOddAxiMaster.reset()
  dcsEvenAxiMaster.reset()

  private def log(msg: String): Unit = println(s"DcsAppMaster: $msg")

  def genLoadStore(addr: BigInt): ClLoadStore = {
    assert(addr % ECI_CL_SIZE_BYTES == 0, "address for cacheline not aligned")

    val aliased = aliasAddress(addr)
    val dcs = if (aliased(7)) dcsOddAxiMaster else dcsEvenAxiMaster

    new ClLoadStore {
      def load: List[Byte] = {
        val ret = dcs.read(aliased, ECI_CL_SIZE_BYTES, len = 1)
        log(f"DCS load:  addr $addr%#x (aliased $aliased%#x) -> ${ret.bytesToHex}")
        ret
      }
      def store(d: List[Byte]): Unit = {
        log(f"DCS store: addr $addr%#x (aliased $aliased%#x) <- ${d.bytesToHex}")
        assert(d.length == ECI_CL_SIZE_BYTES, s"cache-line flush length ${d.length} does not match cacheline size ${ECI_CL_SIZE_BYTES}")
        dcs.write(aliased, d)
      }
    }
  }

  // operates on aliased address!
  def findCl(addr: BigInt): DcsStateMachineSim = {
    val unaliased = unaliasAddress(addr)
    clMap.getOrElseUpdate(addr, new DcsStateMachineSim(f"CL $unaliased%#x", genLoadStore(unaliased)))
  }

  def roundAddr(addr: BigInt): BigInt = addr - (addr & (ECI_CL_SIZE_BYTES - 1))

  /**
   * Read synchronously through the two DCS interfaces, updating the internal cacheline states as it goes along.
   * @param addr start address of read, does not have to be aligned
   * @param totalBytes number of bytes to read
   * @return read data truncated to the right length
   * @note FIXME the DCS doesn't actually handle sub-cacheline access properly (talk to adamt), so we forbid
   *       sub-cacheline access for now
   */
  def read(addr: BigInt, totalBytes: Int): List[Byte] = {
    val builder = new mutable.ArrayBuilder.ofByte

    val roundedAddr = roundAddr(addr)
    val padFront = (addr - roundedAddr).toInt

    // we forbid unaligned access for now
    assert(padFront == 0, f"DCS does not handle sub cacheline access properly yet, $addr%#x needs to be aligned")

    val totalLen = roundUp(padFront + totalBytes, ECI_CL_SIZE_BYTES).toInt
    val numCls = totalLen / ECI_CL_SIZE_BYTES

    (0 until numCls) foreach { idx =>
      val clOffset = roundedAddr + idx * ECI_CL_SIZE_BYTES
      builder ++= findCl(aliasAddress(clOffset)).read
    }

    builder.result().toList.slice(padFront, padFront + totalBytes)
  }

  /**
   * Write synchronously through the two DCS interfaces, updating the internal cacheline states as it goes along.
   * @param addr start of address to write, does not have to be aligned
   * @param data data to write
   * @note FIXME the DCS doesn't actually handle sub-cacheline access properly (talk to adamt), so we forbid
   *       sub-cacheline access for now
   */
  def write(addr: BigInt, data: List[Byte]): Unit = {
    val roundedAddr = roundAddr(addr)
    val padFront = (addr - roundedAddr).toInt

    // we forbid unaligned access for now
    assert(padFront == 0, f"DCS does not handle sub cacheline access properly yet, $addr%#x needs to be aligned")

    val totalLen = roundUp(padFront + data.length, ECI_CL_SIZE_BYTES).toInt
    val padBack = totalLen - padFront - data.length
    val numCls = totalLen / ECI_CL_SIZE_BYTES

    (0 until numCls) foreach { idx =>
      val clOffset = roundedAddr + idx * ECI_CL_SIZE_BYTES
      findCl(aliasAddress(clOffset)).modify { clData =>
        idx match {
          case 0 => clData.take(padFront) ++ data.take(ECI_CL_SIZE_BYTES - padFront)
          case i if i == numCls - 1 => data.takeRight(ECI_CL_SIZE_BYTES - padBack) ++ clData.takeRight(padBack)
          case _ =>
            val start = padFront + (idx - 1) * ECI_CL_SIZE_BYTES
            data.slice(start, start + ECI_CL_SIZE_BYTES)
        }
      }
    }
  }

  Seq(dcsEven, dcsOdd).zipWithIndex.foreach { case (dcs, idx) =>
    val respQueue = mutable.Queue[LclChannel => Unit]()

    var stallLcl = false

    StreamReadyRandomizer(dcs.cleanMaybeInvReq, clockDomain, () => !stallLcl)
    StreamMonitor(dcs.cleanMaybeInvReq, clockDomain) { req =>
      assert(!stallLcl)
      stallLcl = true

      // verify VC numbers
      req.vc.toInt match {
        case 16 => assert(idx == 1)
        case 17 => assert(idx == 0)
      }

      val aliased = req.data.lclMfwdGeneric.simGet(_.address).toBigInt
      val addr = unaliasAddress(aliased)
      val cl = findCl(aliased)

      // deadlock detection: LC/LCI should not be sent when reload is blocked
      // refer to CCKit Figure 7.4
      // FIXME: there's more cases to disallow
      // FIXME: this also disallows when the reload does not depend on LCA/LCIA
      assert(!cl.memInProgress, "potential deadlock/race condition: lc/lci sent during cacheline reload")

      val dmask = req.data.lclMfwdGeneric.simGet(_.dmask).toInt
      assert(dmask == 0xf, f"dmask != 0xf: $dmask%#x")
      assert(req.data.lclMfwdGeneric.simGet(_.ns).toBoolean)
      assert(req.data.lclMfwdGeneric.simGet(_.rnode).toInt == 1)

      // process state change
      val opcode = req.data.lclMfwdGeneric.simGet(_.opcode).toInt
      val hreqId = req.data.lclMfwdGeneric.simGet(_.hreqId).toInt

      opcode match {
        case 0 => // LC
          log(f"LC:   ID $hreqId addr $addr%#x (aliased $aliased%#x)")
          cl.toShared()
        case 1 => // LCI
          log(f"LCI:  ID $hreqId addr $addr%#x (aliased $aliased%#x)")
          cl.toInvalid()
      }
      cl.lock()

      // queue response
      respQueue += { chan =>
        stallLcl = false

        chan.data.lclMrsp0to1.simGet(_.opcode) #= opcode
        chan.data.lclMrsp0to1.simGet(_.hreqId) #= hreqId
        chan.data.lclMrsp0to1.simGet(_.dmask) #= 0xf
        chan.data.lclMrsp0to1.simGet(_.ns) #= true
        chan.data.lclMrsp0to1.simGet(_.address) #= aliased
        chan.data.lclMrsp0to1.commit()
        chan.size #= 1
        chan.vc #= (if (idx == 0) 19 else 18)

        opcode match {
          case 0 => log(f"LCA:  ID $hreqId addr $addr%#x (aliased $aliased%#x)")
          case 1 => log(f"LCIA: ID $hreqId addr $addr%#x (aliased $aliased%#x)")
        }
      }
    }

    StreamDriver(dcs.cleanMaybeInvResp, clockDomain) { chan =>
      if (respQueue.isEmpty) false else {
        respQueue.dequeue()(chan)
        true
      }
    }

    StreamReadyRandomizer(dcs.unlockResp, clockDomain, () => !stallLcl)
    StreamMonitor(dcs.unlockResp, clockDomain) { ul =>
      // verify VC numbers
      ul.vc.toInt match {
        case 18 => assert(idx == 1)
        case 19 => assert(idx == 0)
      }

      val aliased = ul.data.ul.simGet(_.address).toBigInt
      val addr = unaliasAddress(aliased)
      val cl = findCl(aliased)

      assert(ul.data.ul.simGet(_.opcode).toInt == 2)
      log(f"UL:  addr $addr%#x (aliased $aliased%#x)")
      cl.unlock()
    }
  }
}
