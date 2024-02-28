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
case class DcsInterface(axiConfig: Axi4Config) extends Bundle {
  assert(64 to 1024 contains axiConfig.dataWidth, s"DCS desc_to_axi does not support dataWidth ${axiConfig.dataWidth}")

  /** read and write data AXI channel */
  val axi = slave(Axi4(axiConfig))
  /** request channel for clean (LC) and clean-invalidate (LCI) */
  val cleanMaybeInvReq = master(Stream(LclChannel()))
  /** response channel for clean (LCA) and clean-invalidate (LCIA) */
  val cleanMaybeInvResp = slave(Stream(LclChannel()))
  /** response channel for unlock (UL) */
  val unlockResp = master(Stream(LclChannel()))
}
