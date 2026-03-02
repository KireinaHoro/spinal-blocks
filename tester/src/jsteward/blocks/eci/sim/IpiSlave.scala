package jsteward.blocks.eci.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim._
import jsteward.blocks.eci.EciIntcInterface
import jsteward.blocks.misc.sim.StreamMonitorThreadful

/** Simulation slave for interrupt requests from FPGA to CPU.
  *
  * [[preemptHandler]] takes two arguments: core ID, and interrupt ID.
  */
case class IpiSlave(port: Stream[EciIntcInterface], clockDomain: ClockDomain)(preemptHandler: (Int, Int) => Unit) {
  private def log(msg: String): Unit = println(s"IpiSlave:\t$msg")

  StreamMonitorThreadful(port, clockDomain) { preemptReq =>
    val intId = preemptReq.intId.toInt
    val targetList = preemptReq.affLvl0.toInt
    val aff1 = preemptReq.affLvl1.toInt
    val cmd = preemptReq.cmd.toInt
    log(f"IRQ cmd=$cmd INTID=$intId TargetList=$targetList%x Aff1=$aff1")

    assert(cmd == 0, "only command 0 is supported")
    assert(intId >= 8 && intId < 16, "only INTID 8-15 are allowed")
    // re-calculate coreID based on ARM's definition
    assert(Integer.bitCount(targetList) == 1, "we only support one core at a time")

    val coreID = aff1 * 16 + Integer.numberOfTrailingZeros(targetList)
    assert(coreID < 48, "ThunderX only has 48 cores")

    preemptHandler(coreID, intId)
  }
  StreamReadyRandomizer(port, clockDomain)
}
