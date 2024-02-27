package jsteward.blocks.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axis._

import scala.language.postfixOps

case class AxiDmaCmd(dmaConfig: AxiDmaConfig) extends Bundle {

  import dmaConfig._

  val addr = UInt(axiConfig.addressWidth bits)
  val len = UInt(lenWidth bits)
  val tag = UInt(tagWidth bits)
}

case class AxiDmaReadDescStatus(dmaConfig: AxiDmaConfig) extends Bundle {

  import dmaConfig._

  val tag = UInt(tagWidth bits)
  val error = Bits(4 bits)
}

case class AxiDmaWriteDescStatus(dmaConfig: AxiDmaConfig) extends Bundle {

  import dmaConfig._

  val len = UInt(lenWidth bits)
  val tag = UInt(tagWidth bits)
  val id = UInt(intfAxisConfig.idWidth bits)
  val dest = UInt(intfAxisConfig.destWidth bits)
  val user = UInt(intfAxisConfig.userWidth bits)
  val error = Bits(4 bits)
}

case class AxiDmaConfig(axiConfig: Axi4Config,
                        axisConfig: Axi4StreamConfig,
                        axiMaxBurstLen: Int = 16,
                        lenWidth: Int = 20,
                        tagWidth: Int = 8) {
  val intfAxisConfig = mapToIntf(axisConfig)
  val readDescConfig = Axi4StreamCustomConfig.fromStreamConfig(
    payloadType = AxiDmaCmd(this),
    config = intfAxisConfig.copy(useLast = false)
  )

  def readDescBus = Axi4StreamCustom(readDescConfig)

  def readDescStatusBus = Flow(AxiDmaReadDescStatus(this))

  val writeDescConfig = Axi4StreamCustomConfig.fromStreamConfig(
    payloadType = AxiDmaCmd(this),
    config = intfAxisConfig.copy(useLast = false, useDest = false, useUser = false, useId = false)
  )

  def writeDescBus = Axi4StreamCustom(writeDescConfig)

  def writeDescStatusBus = Flow(AxiDmaWriteDescStatus(this))
}

class AxiDma(dmaConfig: AxiDmaConfig,
             enableSG: Boolean = false,
             enableUnaligned: Boolean = false,
            ) extends BlackBox {
  assert(clockDomain.config.resetKind == SYNC, "verilog-axi requires synchronous reset")

  private val axiConfig = dmaConfig.axiConfig
  private val axisConfig = dmaConfig.axisConfig

  val generic = new Generic {
    val AXI_DATA_WIDTH = axiConfig.dataWidth
    val AXI_ADDR_WIDTH = axiConfig.addressWidth
    val AXI_STRB_WIDTH = axiConfig.dataWidth / 8
    val AXI_ID_WIDTH = axiConfig.idWidth
    val AXI_MAX_BURST_LEN = dmaConfig.axiMaxBurstLen
    val AXIS_DATA_WIDTH = axisConfig.dataWidth * 8 // BYTES
    val AXIS_KEEP_ENABLE = axisConfig.useKeep
    val AXIS_KEEP_WIDTH = axisConfig.dataWidth
    val AXIS_LAST_ENABLE = axisConfig.useLast
    val AXIS_ID_ENABLE = axisConfig.useId
    val AXIS_ID_WIDTH = dmaConfig.intfAxisConfig.idWidth
    val AXIS_DEST_ENABLE = axisConfig.useDest
    val AXIS_DEST_WIDTH = dmaConfig.intfAxisConfig.destWidth
    val AXIS_USER_ENABLE = axisConfig.useUser
    val AXIS_USER_WIDTH = dmaConfig.intfAxisConfig.userWidth
    val LEN_WIDTH = dmaConfig.lenWidth
    val TAG_WIDTH = dmaConfig.tagWidth
    val ENABLE_SG = enableSG
    val ENABLE_UNALIGNED = enableUnaligned
  }
  val modName = "axi_dma"
  setBlackBoxName(modName)

  val trimmedAxiConfig = axiConfig.copy(useQos = false, useRegion = false)

  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()

    val s_axis_read_desc = slave(dmaConfig.readDescBus)
    val m_axis_read_desc_status = master(dmaConfig.readDescStatusBus)
    val m_axis_read_data = master(Axi4Stream(dmaConfig.intfAxisConfig))

    val s_axis_write_desc = slave(dmaConfig.writeDescBus)
    val m_axis_write_desc_status = master(dmaConfig.writeDescStatusBus)
    val s_axis_write_data = slave(Axi4Stream(dmaConfig.intfAxisConfig))

    val m_axi = master(Axi4(trimmedAxiConfig))

    val read_enable = in Bool()
    val write_enable = in Bool()
    val write_abort = in Bool()
  }

  lazy val readDataMaster = io.m_axis_read_data.toSpinal(axisConfig)
  lazy val writeDataSlave = io.s_axis_write_data.toSpinal(axisConfig)

  mapCurrentClockDomain(io.clk, io.rst)

  noIoPrefix()

  addPrePopTask { () =>
    renameAxi4IO()
    renameAxi4StreamIO()
  }

  addRTLPath(axiRTLFile(modName))
  addRTLPath(axiRTLFile(s"${modName}_rd"))
  addRTLPath(axiRTLFile(s"${modName}_wr"))
}
