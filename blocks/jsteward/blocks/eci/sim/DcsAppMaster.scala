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

import scala.util.Random

/**
 * Simulation master for an Enzian app using the Directory Controller Slice (DCS) interface; refer to CCKit for more
 * details.  The master mimics memory transfers from the DCS'es and keeps a internal cache-line state machine (in
 * [[DcsStateMachineSim]]).
 * @param dcsEven DCS interface for even cachelines (in aliased addresses)
 * @param dcsOdd DCS interface for odd cachelines (in aliased addresses)
 * @param clockDomain the clock domain of ECI
 * @param voluntaryInvProb probability of a voluntary eviction of some cacheline (picked randomly for cachelines that
 *                         are not invalid)
 * @param maxVoluntaryInvsInRead maximum number of voluntary evictions that could happen in a single read operation.
 *                               A read called from the TB triggers: read, (inv, read, )*
 */
case class DcsAppMaster(dcsEven: DcsInterface, dcsOdd: DcsInterface, clockDomain: ClockDomain,
                        voluntaryInvProb: Double = 0.01,
                        maxVoluntaryInvsInRead: Int = 5,
                        ) {
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

    val totalLen = roundUp(padFront + totalBytes, ECI_CL_SIZE_BYTES).toInt
    val numCls = totalLen / ECI_CL_SIZE_BYTES

    def readWithRandomInv(clOffset: BigInt): List[Byte] = {
      val clState = findCl(aliasAddress(clOffset))
      val firstRead = clState.read
      val numReps = Random.nextInt(maxVoluntaryInvsInRead)
      (0 until numReps) foreach { _ =>
        clState.invalidate()
        val reread = clState.read
        assert(reread == firstRead,
          s"""CL voluntary reload mismatch:
             |first read: "${firstRead.bytesToHex}"
             |reread:     "${reread.bytesToHex}"
             |""".stripMargin)

        clockDomain.waitActiveEdge(Random.nextInt(20))
      }

      firstRead
    }

    (0 until numCls) foreach { idx =>
      val clOffset = roundedAddr + idx * ECI_CL_SIZE_BYTES
      builder ++= readWithRandomInv(clOffset)
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
    def writePartial(data: List[Byte], offset: BigInt): Unit = {
      val padFront = (addr - roundedAddr + offset).toInt

      val totalLen = roundUp(padFront + data.length, ECI_CL_SIZE_BYTES).toInt
      val padBack = totalLen - padFront - data.length
      val numCls = totalLen / ECI_CL_SIZE_BYTES

      (0 until numCls) foreach { idx =>
        val clOffset = roundedAddr + idx * ECI_CL_SIZE_BYTES
        val clState = findCl(aliasAddress(clOffset))

        clState.modify { clData =>
          if (numCls == 1) {
            clData.take(padFront) ++ data ++ clData.takeRight(padBack)
          } else idx match {
            case 0 => clData.take(padFront) ++ data.take(ECI_CL_SIZE_BYTES - padFront)
            case i if i == numCls - 1 => data.takeRight(ECI_CL_SIZE_BYTES - padBack) ++ clData.takeRight(padBack)
            case _ =>
              val start = padFront + (idx - 1) * ECI_CL_SIZE_BYTES
              data.slice(start, start + ECI_CL_SIZE_BYTES)
          }
        }

        clState.invalidate()
      }
    }

    var start = 0
    while (start < data.length) {
      val sliceLen = Random.nextInt(data.length - 1) + 1

      writePartial(data.slice(start, start + sliceLen), start)
      start += sliceLen
    }

    // check if written data reflects actual write
    val readback = read(addr, data.length)
    assert(readback == data,
      s"""CL write readback mismatch:
         |written:  "${data.bytesToHex}"
         |readback: "${readback.bytesToHex}"
         |""".stripMargin)
  }

  Seq(dcsEven, dcsOdd).zipWithIndex.foreach { case (dcs, idx) =>
    val respQueue = mutable.Queue[LclChannel => Unit]()

    StreamReadyRandomizer(dcs.cleanMaybeInvReq, clockDomain)
    StreamMonitor(dcs.cleanMaybeInvReq, clockDomain) { req =>
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

    StreamReadyRandomizer(dcs.unlockResp, clockDomain)
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

  // voluntary invalidation from CPU
  clockDomain.onSamplings {
    val targets = clMap.iterator.filter(_._2.state != EciClStates.Invalid)
    if (Random.nextDouble <= voluntaryInvProb && targets.nonEmpty) {
      val (aliased, clState) = choose(targets, Random)
      log(f"Voluntary invalidation: ${unaliasAddress(aliased)}%#x (aliased $aliased%#x)")

      clState.invalidate()
    }
  }
}
