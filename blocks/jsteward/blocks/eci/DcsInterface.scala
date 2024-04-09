package jsteward.blocks.eci

import spinal.core._
import spinal.lib.bus.amba4.axi._
import spinal.lib._

case class LclChannel() extends Bundle {
  import jsteward.blocks.eci.EciCmdDefs._

  val data = EciWord()
  val size = EciPacketSize
  val vc = EciVcSize
}

/** Interface with dcs_2_axi.sv (DCS with read and write data wrapped in AXI) */
case class DcsInterface(axiConfig: Axi4Config) extends Bundle with IMasterSlave {
  assert(64 to 1024 contains axiConfig.dataWidth, s"DCS desc_to_axi does not support dataWidth ${axiConfig.dataWidth}")

  /** read and write data AXI channel */
  val axi = Axi4(axiConfig)
  /** request channel for clean (LC) and clean-invalidate (LCI) */
  val cleanMaybeInvReq = Stream(LclChannel())
  /** response channel for clean (LCA) and clean-invalidate (LCIA) */
  val cleanMaybeInvResp = Stream(LclChannel())
  /** response channel for unlock (UL) */
  val unlockResp = Stream(LclChannel())

  override def asMaster(): Unit = {
    master(axi)
    slave(cleanMaybeInvReq)
    master(cleanMaybeInvResp)
    slave(unlockResp)
  }

  override def clone: DcsInterface = DcsInterface(axiConfig)
}
