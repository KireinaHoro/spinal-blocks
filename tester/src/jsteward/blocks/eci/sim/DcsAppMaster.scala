package jsteward.blocks.eci.sim

import jsteward.blocks.eci.EciCmdDefs.ECI_CL_SIZE_BYTES
import jsteward.blocks.eci._
import jsteward.blocks.misc.sim._
import spinal.core.sim._
import spinal.core.{ClockDomain, IntToBuilder, roundUp}
import spinal.lib.{BigIntRicher => _, _}
import spinal.lib.bus.amba4.axi.sim.Axi4Master
import spinal.lib.sim._

import scala.collection.mutable
import scala.util.Random

/**
 * Simulation master for an Enzian app using the Directory Controller Slice (DCS) interface; refer to CCKit for more
 * details.  The master mimics memory transfers from the DCS'es and keeps a internal cache-line state machine (in
 * [[DcsStateMachineSim]]).
 *
 * @param dcsEven                DCS interface for odd cachelines (in aliased addresses; odd means the VC NUMBER is odd)
 * @param dcsOdd                 DCS interface for even cachelines (in aliased addresses; even means the VC NUMBER is even)
 * @param clockDomain            the clock domain of ECI
 * @param voluntaryInvProb       probability of a voluntary eviction of some cacheline (picked randomly for cachelines that
 *                               are not invalid)
 * @param maxVoluntaryInvsInRead maximum number of voluntary evictions that could happen in a single read operation.
 *                               A read called from the TB triggers: read, (inv, read, )*
 */
case class DcsAppMaster(dcsEven: DcsInterface, dcsOdd: DcsInterface, clockDomain: ClockDomain,
                        var voluntaryInvProb: Double = 0.01,
                        var doPartialWrite: Boolean = true,
                        var doReadbackCheck: Boolean = false,
                        maxVoluntaryInvsInRead: Int = 5,
                        maxInflightReqsPerSlice: Int = 5,
                        dcuIdxWidth: Int = 5, // default 32 DCUs per slice
                       ) {
  // index is unaliased address
  private val clMap = mutable.HashMap[BigInt, DcsStateMachineSim]()
  private val dcsOddAxiMaster = Axi4Master(dcsOdd.axi, clockDomain, "dcsOdd")
  private val dcsEvenAxiMaster = Axi4Master(dcsEven.axi, clockDomain, "dcsEven")

  dcsOddAxiMaster.reset()
  dcsEvenAxiMaster.reset()

  private def log(msg: String): Unit = println(s"DcsAppMaster:\t$msg")

  val dcusInProgress = mutable.Seq.fill(1 << (dcuIdxWidth + 1))(0)
  private def dcuToggle(id: Int, flagBit: Int) = dcusInProgress(id) ^= flagBit
  private def dcuCond(id: Int, flagBit: Int) = (dcusInProgress(id) & flagBit) != 0
  private def dcuEnter(id: Int, flagBit: Int) = {
    // XXX: no need to lock since only one SimThread is running at a time
    if (dcuCond(id, flagBit)) {
      log(f"DCU#$id busy for read, waiting...")
      waitUntil(!dcuCond(id, flagBit))
    }
    dcuToggle(id, flagBit)
  }
  private def dcuExit(id: Int, flagBit: Int) = dcuToggle(id, flagBit)

  private def dcuRdEnter(id: Int) = dcuEnter(id, 1)
  private def dcuRdExit(id: Int) = dcuExit(id, 1)
  private def dcuInRd(id: Int) = dcuCond(id, 1)
  private def dcuWrEnter(id: Int) = dcuEnter(id, 2)
  private def dcuWrExit(id: Int) = dcuExit(id, 2)

  private val dcsInflightLeft = mutable.Seq.fill(2)(maxInflightReqsPerSlice)
  private def dcsInflightEnter(isEven: Boolean) = {
    val dcsId = if (isEven) 0 else 1
    val sliceName = if (isEven) "even" else "odd "
    def cond = dcsInflightLeft(dcsId) <= 0
    if (cond) {
      log(f"DCS $sliceName busy for read, waiting...")
      waitUntil(!cond)
    }
    dcsInflightLeft(dcsId) -= 1
  }
  private def dcsInflightExit(isEven: Boolean) = {
    val dcsId = if (isEven) 0 else 1
    dcsInflightLeft(dcsId) += 1
  }

  private def genLoadStore(addr: BigInt): ClLoadStore = {
    assert(addr % ECI_CL_SIZE_BYTES == 0, "address for cacheline not aligned")

    val aliased = aliasAddress(addr)
    // odd dcs for even cacheline!
    val isEven = aliased(7)
    val dcs = if (isEven) dcsEvenAxiMaster else dcsOddAxiMaster
    val dcuId = aliased(dcuIdxWidth + 7 downto 7).toInt // INCLUDES odd/even bit (not DCU_IDX)

    val len = EciCmdDefs.ECI_CL_WIDTH / dcsEven.axiConfig.dataWidth - 1

    new ClLoadStore {
      def load: List[Byte] = {
        var inProgress = true
        dcuRdEnter(dcuId)
        dcsInflightEnter(isEven)

        fork {
          clockDomain.waitActiveEdge(100000)
          assert(!inProgress, f"timeout waiting for CL reload on addr $addr%#x (aliased $aliased%#x)")
        }

        val ret = dcs.read(aliased, ECI_CL_SIZE_BYTES, len = len, id = dcuId)
        log(f"DCS load (DCU#$dcuId):  addr $addr%#x (aliased $aliased%#x) -> ${ret.bytesToHex}")
        inProgress = false

        dcsInflightExit(isEven)
        dcuRdExit(dcuId)
        ret
      }

      def store(d: List[Byte]): Unit = {
        dcuWrEnter(dcuId)
        // axi_dw_converter does not limit in-flight writes, so no need to enter

        log(f"DCS store (DCU#$dcuId): addr $addr%#x (aliased $aliased%#x) <- ${d.bytesToHex}")
        assert(d.length == ECI_CL_SIZE_BYTES, s"cache-line flush length ${d.length} does not match cacheline size ${ECI_CL_SIZE_BYTES}")
        dcs.write(aliased, d, maxLen = len, id = dcuId)

        dcuWrExit(dcuId)
      }
    }
  }

  // operates on aliased address!
  private def findCl(addr: BigInt): DcsStateMachineSim = {
    val unaliased = unaliasAddress(addr)
    clMap.getOrElseUpdate(addr, DcsStateMachineSim(f"CL $unaliased%#x", genLoadStore(unaliased)))
  }

  private def roundAddr(addr: BigInt): BigInt = addr - (addr & (ECI_CL_SIZE_BYTES - 1))

  /**
   * Read synchronously through the two DCS interfaces, updating the internal cacheline states as it goes along.
   *
   * @param addr       start address of read, does not have to be aligned
   * @param totalBytes number of bytes to read
   * @param doInvIdemptCheck whether we should enforce voluntary invalidation and then reload on same addr should
   *                         always have same result as first read
   * @return read data truncated to the right length
   * @note FIXME the DCS doesn't actually handle sub-cacheline access properly (talk to adamt), so we forbid
   *       sub-cacheline access for now
   */
  def read(addr: BigInt, totalBytes: Int, doInvIdemptCheck: Boolean = true): List[Byte] = {
    val builder = new mutable.ArrayBuilder.ofByte

    val roundedAddr = roundAddr(addr)
    val padFront = (addr - roundedAddr).toInt

    val totalLen = roundUp(padFront + totalBytes, ECI_CL_SIZE_BYTES).toInt
    val numCls = totalLen / ECI_CL_SIZE_BYTES

    def readWithRandomInv(clOffset: BigInt): List[Byte] = {
      println(f"Reading $clOffset%#x as part of host read request to $addr%#x ($totalBytes bytes)")
      val aliasedAddr = aliasAddress(clOffset)
      val clState = findCl(aliasedAddr)
      val firstRead = clState.read
      val numReps = if (voluntaryInvProb < Double.MinPositiveValue) 0 else Random.nextInt(maxVoluntaryInvsInRead)
      println(f"Read on $clOffset%#x will trigger $numReps voluntary reloads")
      (0 until numReps) foreach { idx =>
        log(f"Voluntary invalidation #$idx DURING READ: $clOffset%#x (aliased $aliasedAddr%#x)")

        clState.invalidate()
        val reread = clState.read
        if (doInvIdemptCheck) {
          assert(reread == firstRead,
            s"""CL voluntary reload mismatch:
               |first read: "${firstRead.bytesToHex}"
               |reread:     "${reread.bytesToHex}"
               |""".stripMargin)
        } else {
          log(f"Voluntary invalidation idempotency check skipped!")
        }

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
   *
   * @param addr start of address to write, does not have to be aligned
   * @param data data to write
   * @note FIXME the DCS doesn't actually handle sub-cacheline access properly (talk to adamt), so we forbid
   *       sub-cacheline access for now
   */
  def write(addr: BigInt, data: List[Byte]): Unit = {
    def writePartial(data: List[Byte], offset: BigInt): Unit = {
      val startAddr = addr + offset
      val roundedStartAddr = roundAddr(startAddr)
      val padFront = (startAddr - roundedStartAddr).toInt
      val totalLen = roundUp(padFront + data.length, ECI_CL_SIZE_BYTES).toInt
      val padBack = totalLen - padFront - data.length
      val numCls = totalLen / ECI_CL_SIZE_BYTES

      (0 until numCls) foreach { idx =>
        val currCl = roundedStartAddr + idx * ECI_CL_SIZE_BYTES
        val clState = findCl(aliasAddress(currCl))

        clState.modify { clData =>
          if (numCls == 1) {
            clData.take(padFront) ++ data ++ clData.takeRight(padBack)
          } else idx match {
            case 0 => clData.take(padFront) ++ data.take(ECI_CL_SIZE_BYTES - padFront)
            case i if i == numCls - 1 => data.takeRight(ECI_CL_SIZE_BYTES - padBack) ++ clData.takeRight(padBack)
            case _ =>
              val start = idx * ECI_CL_SIZE_BYTES - padFront
              data.slice(start, start + ECI_CL_SIZE_BYTES)
          }
        }
      }
    }

    var start = 0
    while (start < data.length) {
      val sliceLen = if (doPartialWrite) Random.nextInt(data.length) + 1 else data.length
      if (sliceLen < data.length)
        log(s"Partial write and voluntary invalidation: start $start len $sliceLen")

      writePartial(data.slice(start, start + sliceLen), start)
      start += sliceLen
    }

    if (doReadbackCheck) {
      // check if written data reflects actual write
      val readback = read(addr, data.length)
      assert(readback == data,
        f"""CL $addr%#x write readback mismatch:
           |written:  "${data.bytesToHex}"
           |readback: "${readback.bytesToHex}"
           |""".stripMargin)
    }
  }

  /**
   * Force invalidation of a cache line.  Useful for reproducing DC traces captured in HW to catch
   * corner cases in the 2F2F state machine.
   * @param addr address of any word in the cacheline to be dropped (will be rounded)
   */
  def drop(addr: BigInt): Unit = {
    val clAddr = roundAddr(addr)
    findCl(aliasAddress(clAddr)).invalidate()
  }

  /**
    * Compare and swap **one byte** at the given address.  This is used to simulate the
    * preempt critical region algorithm.
    * @param addr byte address of the byte to operate on
    * @param oldVal old value that is read out separately through [[read]]
    * @param newVal new value to update into destination
    * @return if the operation succeeded or not.
    */
  def casByte(addr: BigInt, oldVal: Int, newVal: Int): Boolean = {
    val roundedAddr = roundAddr(addr)
    val byteOffset = (addr - roundedAddr).toInt
    val clState = findCl(aliasAddress(roundedAddr))

    // upgrade to modified and check if value is same as old val
    var ret = true
    clState.modify { clData =>
      if (clData(byteOffset) != oldVal) {
        log(f"CAS $addr%#x failed!    old=$oldVal new=$newVal")
        ret = false
        clData
      } else {
        log(f"CAS $addr%#x succeeded! old=$oldVal new=$newVal")
        clData.updated(byteOffset, newVal.toByte)
      }
    }

    ret
  }

  Seq(dcsEven, dcsOdd).zipWithIndex.foreach { case (dcs, idx) =>
    val respQueue = mutable.Queue[LclChannel => Unit]()

    StreamReadyRandomizer(dcs.cleanMaybeInvReq, clockDomain)
    StreamMonitorThreadful(dcs.cleanMaybeInvReq, clockDomain) { req =>
      // verify VC numbers odd/even
      req.vc.toInt match {
        case 16 => assert(idx == 0, "VC number and odd/even mismatch")
        case 17 => assert(idx == 1, "VC number and odd/even mismatch")
      }

      val aliased = req.data.lclMfwdGeneric.simGet(_.address).toBigInt
      // verify odd/even address: odd DCS should have even addresses

      val addr = unaliasAddress(aliased)
      val cl = findCl(aliased)

      val dmask = req.data.lclMfwdGeneric.simGet(_.dmask).toInt
      assert(dmask == 0xf, f"dmask != 0xf: $dmask%#x")
      assert(req.data.lclMfwdGeneric.simGet(_.ns).toBoolean)
      assert(req.data.lclMfwdGeneric.simGet(_.rnode).toInt == 1)

      // process state change
      val opcode = req.data.lclMfwdGeneric.simGet(_.opcode).toInt
      val hreqId = req.data.lclMfwdGeneric.simGet(_.hreqId).toInt

      // deadlock detection: LC/LCI should not be sent when reload is blocked
      // refer to CCKit Figure 7.4
      // FIXME: there's more cases to disallow
      // FIXME: this also disallows when the reload does not depend on LCA/LCIA
      if (cl.memInProgress) {
        log(s"!!! potential deadlock/race condition !!! lc/lci sent for ${cl.id} during cacheline reload")
        clockDomain.waitActiveEdgeWhere(!cl.memInProgress)
      }

      val opName = opcode match {
        case 0 => "LC "
        case 1 => "LCI"
      }
      log(f"$opName: ID $hreqId addr $addr%#x (aliased $aliased%#x)")
      assert(aliased(7) == (idx == 0), f"DCS even/odd mismatch: got aliased address $aliased%#x but DCS index is $idx")

      opcode match {
        case 0 => cl.toShared()
        case 1 => cl.toInvalid()
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
        chan.vc #= (if (idx == 0) 18 else 19)

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
    StreamMonitorThreadful(dcs.unlockResp, clockDomain) { ul =>
      // verify VC numbers
      ul.vc.toInt match {
        case 18 => assert(idx == 0)
        case 19 => assert(idx == 1)
      }

      val aliased = ul.data.ul.simGet(_.address).toBigInt
      // verify odd/even address: odd DCS should have even addresses
      assert(aliased(7) == (idx == 0), f"DCS even/odd mismatch: got aliased address $aliased%#x but DCS index is $idx")

      val addr = unaliasAddress(aliased)
      val cl = findCl(aliased)

      assert(ul.data.ul.simGet(_.opcode).toInt == 2)
      log(f"UL:  addr $addr%#x (aliased $aliased%#x)")
      cl.unlock()
    }
  }

  // voluntary invalidation from CPU
  fork {
    while (true) {
      val targets = clMap.iterator.filter(_._2.state != EciClStates.Invalid)
      if (Random.nextDouble <= voluntaryInvProb && targets.nonEmpty) {
        val (aliased, clState) = choose(targets, Random)
        if (!clState.inTransition) {
          log(f"Voluntary invalidation: ${unaliasAddress(aliased)}%#x (aliased $aliased%#x)")

          clState.invalidate()
        }
      }

      clockDomain.waitActiveEdge()
    }
  }
}
