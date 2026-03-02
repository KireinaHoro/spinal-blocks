package jsteward.blocks.axi

import jsteward.blocks.misc.DriveMissingVec
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.misc.SizeMapping

object AxiCrossbarRegType extends Enumeration {
  type AxiCrossbarRegType = Value
  val Bypass, SimpleBuffer, SkidBuffer = Value
}
import AxiCrossbarRegType._

case class AxiCrossbarSlaveConfig(
    threads: Int = 2,
    concurrentOps: Int = 16,
    awRegType: AxiCrossbarRegType = Bypass,
    wRegType: AxiCrossbarRegType = Bypass,
    bRegType: AxiCrossbarRegType = SimpleBuffer,
    arRegType: AxiCrossbarRegType = SimpleBuffer,
    rRegType: AxiCrossbarRegType = SkidBuffer,
)

case class AxiCrossbarMasterConfig(
    regions: Seq[SizeMapping],
    readFromSlaves: Seq[Boolean], // must equal number of slave ports
    writeFromSlaves: Seq[Boolean], // must equal number of slave ports
    concurrentOps: Int = 4,
    isSecure: Boolean = false,
    awRegType: AxiCrossbarRegType = SimpleBuffer,
    wRegType: AxiCrossbarRegType = SkidBuffer,
    bRegType: AxiCrossbarRegType = Bypass,
    arRegType: AxiCrossbarRegType = SimpleBuffer,
    rRegType: AxiCrossbarRegType = Bypass,
)

class AxiCrossbar(
    slaveAxiConfig: Axi4Config, // ID width for slave
    slaveConfigs: Seq[AxiCrossbarSlaveConfig],
    masterConfigs: Seq[AxiCrossbarMasterConfig],
) extends BlackBox {
  val numSlaves = slaveConfigs.length
  val numMasters = masterConfigs.length

  val masterIdWidth = slaveAxiConfig.idWidth + log2Up(numSlaves)
  val slaveIntfConfig = mapToIntf(slaveAxiConfig)

  val masterAxiConfig = slaveAxiConfig.copy(idWidth = masterIdWidth)
  val masterIntfConfig = mapToIntf(masterAxiConfig, addRegion = true)

  assert(slaveAxiConfig.useStrb)
  assert(slaveAxiConfig.useId)
  masterConfigs foreach { mc =>
    assert(mc.readFromSlaves.length == numSlaves)
    assert(mc.writeFromSlaves.length == numSlaves)
    mc.regions foreach { r => assert(r.isAligned) }
  }

  val maxNumRegions = masterConfigs.map(_.regions.length).max
  val paddedRegions = masterConfigs.map(_.regions.padTo(maxNumRegions, SizeMapping(0, 0)))
  val allRegions = paddedRegions.flatten

  val generic = new Generic {
    val S_COUNT = numSlaves
    val M_COUNT = numMasters
    val DATA_WIDTH = slaveIntfConfig.dataWidth
    val ADDR_WIDTH = slaveIntfConfig.addressWidth
    // STRB is always used
    val S_ID_WIDTH = slaveIntfConfig.idWidth
    val M_ID_WIDTH = masterIdWidth
    val AWUSER_ENABLE = slaveAxiConfig.useAwUser
    val AWUSER_WIDTH = slaveIntfConfig.awUserWidth
    val WUSER_ENABLE = slaveAxiConfig.useWUser
    val WUSER_WIDTH = slaveIntfConfig.wUserWidth
    val BUSER_ENABLE = slaveAxiConfig.useBUser
    val BUSER_WIDTH = slaveIntfConfig.bUserWidth
    val ARUSER_ENABLE = slaveAxiConfig.useArUser
    val ARUSER_WIDTH = slaveIntfConfig.arUserWidth
    val RUSER_ENABLE = slaveAxiConfig.useRUser
    val RUSER_WIDTH = slaveIntfConfig.rUserWidth
    val S_THREADS = packParamList(slaveConfigs.map(_.threads))
    val S_ACCEPT = packParamList(slaveConfigs.map(_.concurrentOps))
    val M_REGIONS = maxNumRegions
    val M_BASE_ADDR = packParamList(allRegions.map(_.base), ADDR_WIDTH)
    val M_ADDR_WIDTH = packParamList(allRegions.map { r => log2Up(r.size - 1) })
    val M_CONNECT_READ = packParamList(masterConfigs.flatMap(_.readFromSlaves).map(_.toInt), 1)
    val M_CONNECT_WRITE = packParamList(masterConfigs.flatMap(_.writeFromSlaves).map(_.toInt), 1)
    val M_ISSUE = packParamList(masterConfigs.map(_.concurrentOps))
    val M_SECURE = packParamList(masterConfigs.map(_.isSecure.toInt), 1)
    val S_AW_REG_TYPE = packParamList(slaveConfigs.map(_.awRegType.id), 2)
    val S_W_REG_TYPE = packParamList(slaveConfigs.map(_.wRegType.id), 2)
    val S_B_REG_TYPE = packParamList(slaveConfigs.map(_.bRegType.id), 2)
    val S_AR_REG_TYPE = packParamList(slaveConfigs.map(_.arRegType.id), 2)
    val S_R_REG_TYPE = packParamList(slaveConfigs.map(_.rRegType.id), 2)
    val M_AW_REG_TYPE = packParamList(masterConfigs.map(_.awRegType.id), 2)
    val M_W_REG_TYPE = packParamList(masterConfigs.map(_.wRegType.id), 2)
    val M_B_REG_TYPE = packParamList(masterConfigs.map(_.bRegType.id), 2)
    val M_AR_REG_TYPE = packParamList(masterConfigs.map(_.arRegType.id), 2)
    val M_R_REG_TYPE = packParamList(masterConfigs.map(_.rRegType.id), 2)
  }

  val modName = "axi_crossbar"
  setBlackBoxName(modName)

  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()
  }

  val s_axi = new DriveMissingVec(Axi4(slaveAxiConfig), slave(Axi4(slaveIntfConfig)), numSlaves)
  val m_axi = new DriveMissingVec(Axi4(masterAxiConfig), master(Axi4(masterIntfConfig)), numMasters)

  mapCurrentClockDomain(io.clk, io.rst)
  noIoPrefix()

  addPrePopTask { () =>
    renameAxi4IO()
  }

  addRTLPath(axiRTLFile(modName))
  addRTLPath(axiRTLFile(s"${modName}_rd"))
  addRTLPath(axiRTLFile(s"${modName}_wr"))
  addRTLPath(axiRTLFile(s"${modName}_addr"))
  addRTLPath(axiRTLFile("axi_register_rd"))
  addRTLPath(axiRTLFile("axi_register_wr"))
}
