package jsteward.blocks.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._

class AxiDpRam(
                axiConfig: Axi4Config,
                aClockDomain: ClockDomain = ClockDomain.current,
                bClockDomain: ClockDomain = ClockDomain.current,
                aPipelineOutput: Boolean = false,
                bPipelineOutput: Boolean = false,
                aInterleave: Boolean = false,
                bInterleave: Boolean = false,
              ) extends BlackBox {
  assert(clockDomain.config.resetKind == SYNC, "verilog-axi requires synchronous reset")

  val generic = new Generic {
    val DATA_WIDTH = axiConfig.dataWidth
    val ADDR_WIDTH = axiConfig.addressWidth
    val STRB_WIDTH = axiConfig.dataWidth / 8
    val ID_WIDTH = axiConfig.idWidth
    val A_PIPELINE_OUTPUT = aPipelineOutput
    val B_PIPELINE_OUTPUT = bPipelineOutput
    val A_INTERLEAVE = aInterleave
    val B_INTERLEAVE = bInterleave
  }
  val modName = "axi_dp_ram"
  setBlackBoxName(modName)

  val trimmedAxiConfig = axiConfig.copy(useQos = false, useRegion = false)

  val io = new Bundle {
    val a_clk = in Bool()
    val a_rst = in Bool()

    val b_clk = in Bool()
    val b_rst = in Bool()

    val s_axi_a = slave(Axi4(trimmedAxiConfig)).addTag(ClockDomainTag(aClockDomain))
    val s_axi_b = slave(Axi4(trimmedAxiConfig)).addTag(ClockDomainTag(bClockDomain))
  }

  mapClockDomain(aClockDomain, io.a_clk, io.a_rst)
  mapClockDomain(bClockDomain, io.b_clk, io.b_rst)

  noIoPrefix()

  addPrePopTask { () =>
    renameAxi4IO
  }

  addRTLPath(axiRTLFile(modName))
  addRTLPath(axiRTLFile("axi_ram_wr_rd_if"))
  addRTLPath(axiRTLFile("axi_ram_wr_if"))
  addRTLPath(axiRTLFile("axi_ram_rd_if"))
}
