package jsteward.blocks.axi

import jsteward.blocks.misc.DriveMissing
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._

class AxiAdapter(inConfig: Axi4Config, outDataWidth: Int, convertBurst: Boolean = true, convertNarrowBurst: Boolean = false, forwardId: Boolean = false) extends BlackBox {
  assert(clockDomain.config.resetKind == SYNC, "verilog-axi requires synchronous reset")

  val intfConfig = inConfig.copy(
    awUserWidth = 1,
    arUserWidth = 1,
    wUserWidth = 1,
    rUserWidth = 1,
    bUserWidth = 1,
  )

  val generic = new Generic {
    val ADDR_WIDTH = inConfig.addressWidth
    val S_DATA_WIDTH = inConfig.dataWidth
    val S_STRB_WIDTH = inConfig.dataWidth / 8
    val M_DATA_WIDTH = outDataWidth
    val M_STRB_WIDTH = outDataWidth / 8
    val ID_WIDTH = if (inConfig.useId) inConfig.idWidth else 0
    val AWUSER_ENABLE = inConfig.useAwUser
    val AWUSER_WIDTH = intfConfig.awUserWidth
    val WUSER_ENABLE = inConfig.useWUser
    val WUSER_WIDTH = intfConfig.wUserWidth
    val BUSER_ENABLE = inConfig.useBUser
    val BUSER_WIDTH = intfConfig.bUserWidth
    val ARUSER_ENABLE = inConfig.useArUser
    val ARUSER_WIDTH = intfConfig.arUserWidth
    val RUSER_ENABLE = inConfig.useRUser
    val RUSER_WIDTH = intfConfig.rUserWidth
    val CONVERT_BURST = convertBurst
    val CONVERT_NARROW_BURST = convertNarrowBurst
    val FORWARD_ID = forwardId
  }
  val modName = "axi_adapter"
  setBlackBoxName(modName)

  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()
  }

  val s_axi = new DriveMissing(Axi4(inConfig), slave(Axi4(intfConfig)))
  val m_axi = new DriveMissing(Axi4(inConfig.copy(dataWidth = outDataWidth)), master(Axi4(intfConfig.copy(dataWidth = outDataWidth))))

  mapCurrentClockDomain(io.clk, io.rst)
  noIoPrefix()
  addPrePopTask { () =>
    renameAxi4IO()
  }

  addRTLPath(axiRTLFile(modName))
  addRTLPath(axiRTLFile(s"${modName}_rd"))
  addRTLPath(axiRTLFile(s"${modName}_wr"))
}
