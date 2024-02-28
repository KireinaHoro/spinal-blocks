package jsteward.blocks.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._

class AxiLiteAdapter(inConfig: AxiLite4Config, outDataWidth: Int) extends BlackBox {
  assert(clockDomain.config.resetKind == SYNC, "verilog-axi requires synchronous reset")

  val generic = new Generic {
    val ADDR_WIDTH = inConfig.addressWidth
    val S_DATA_WIDTH = inConfig.dataWidth
    val S_STRB_WIDTH = inConfig.dataWidth / 8
    val M_DATA_WIDTH = outDataWidth
    val M_STRB_WIDTH = outDataWidth / 8
  }
  val modName = "axil_adapter"
  setBlackBoxName(modName)

  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()

    val s_axil = slave(AxiLite4(inConfig))
    val m_axil = master(AxiLite4(inConfig.copy(dataWidth = outDataWidth)))
  }

  mapCurrentClockDomain(io.clk, io.rst)
  noIoPrefix()
  addPrePopTask { () =>
    renameAxi4IO()
  }

  addRTLPath(axiRTLFile(modName))
  addRTLPath(axiRTLFile(s"${modName}_rd"))
  addRTLPath(axiRTLFile(s"${modName}_wr"))
}
