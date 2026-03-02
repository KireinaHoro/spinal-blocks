package jsteward.blocks.axi

import spinal.core._
import spinal.lib.bus.amba4.axilite._
import spinal.lib._

class AxiLiteCdc(config: AxiLite4Config, pushClock: ClockDomain, popClock: ClockDomain) extends BlackBox {
  assert(pushClock.config.resetKind == SYNC, "verilog-axi requires synchronous reset")
  assert(popClock.config.resetKind == SYNC, "verilog-axi requires synchronous reset")

  val generic = new Generic {
    val DATA_WIDTH = config.dataWidth
    val ADDR_WIDTH = config.addressWidth
    val STRB_WIDTH = config.dataWidth / 8
  }
  val modName = "axil_cdc"
  setBlackBoxName(modName)

  val io = new Bundle {
    val s_clk = in Bool()
    val s_rst = in Bool()
    val s_axil = slave(AxiLite4(config)) addTag ClockDomainTag(pushClock)

    val m_clk = in Bool()
    val m_rst = in Bool()
    val m_axil = master(AxiLite4(config)) addTag ClockDomainTag(popClock)
  }

  mapClockDomain(pushClock, io.s_clk, io.s_rst)
  mapClockDomain(popClock, io.m_clk, io.m_rst)
  noIoPrefix()
  addPrePopTask { () =>
    renameAxi4IO()
  }

  addRTLPath(axiRTLFile(modName))
  addRTLPath(axiRTLFile(s"${modName}_rd"))
  addRTLPath(axiRTLFile(s"${modName}_wr"))
}
