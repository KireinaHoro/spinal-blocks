package jsteward.blocks.eci

import spinal.core._
import spinal.lib.bus.amba4.axi._
import spinal.lib._

import scala.language.postfixOps

case class EciChannel() extends Bundle {
  import jsteward.blocks.eci.EciCmdDefs._

  val data = EciWord()
  val size = EciPacketSize
  val vc = EciVcSize
}

case class TracePort() extends Bundle {
  val error = Bool()
  val cli = Bits(40 bits)
  val state = Bits(7 bits)
  val action = Bits(4 bits)
  val request = Bits(5 bits)
}

/** Interface with dcs_2_axi.sv (DCS with read and write data wrapped in AXI) */
case class DcsInterface(axiConfig: Axi4Config) extends Bundle with IMasterSlave {
  assert(64 to 1024 contains axiConfig.dataWidth, s"DCS desc_to_axi does not support dataWidth ${axiConfig.dataWidth}")

  /** read and write data AXI channel */
  val axi = Axi4(axiConfig)
  /** request channel for clean (LC) and clean-invalidate (LCI) */
  val cleanMaybeInvReq = Stream(EciChannel())
  /** response channel for clean (LCA) and clean-invalidate (LCIA) */
  val cleanMaybeInvResp = Stream(EciChannel())
  /** response channel for unlock (UL) */
  val unlockResp = Stream(EciChannel())
  /** trace interface */
  val tracing = Vec(Flow(TracePort()), 2)

  override def asMaster(): Unit = {
    master(axi)
    slave(cleanMaybeInvReq)
    master(cleanMaybeInvResp)
    slave(unlockResp)
    tracing.foreach(master(_))
  }

  override def clone: DcsInterface = DcsInterface(axiConfig)
}
