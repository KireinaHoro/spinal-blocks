package jsteward.blocks.axi

import jsteward.blocks.misc.{DriveMissing, DriveMissingVec}
import spinal.core._
import spinal.lib.bus.amba4.axis._
import spinal.lib._

import scala.language.postfixOps

class AxiStreamDemux(
                    axisConfig: Axi4StreamConfig,
                    numMasterPorts: Int = 4,
                    tdestRoute: Boolean = false,
                    ) extends BlackBox {
  assert(clockDomain.config.resetKind == SYNC, "verilog-axi requires synchronous reset")

  val slaveAxisConfig = axisConfig.copy(idWidth = axisConfig.idWidth + log2Up(numMasterPorts))
  val intfAxisConfig = mapToIntf(axisConfig)
  val slaveIntfAxisConfig = mapToIntf(slaveAxisConfig)
  val generic = new Generic {
    val M_COUNT =  numMasterPorts
    val DATA_WIDTH = axisConfig.dataWidth * 8
    val KEEP_ENABLE = axisConfig.useKeep
    val KEEP_WIDTH = axisConfig.dataWidth
    val ID_ENABLE = axisConfig.useId
    val ID_WIDTH = intfAxisConfig.idWidth
    val DEST_ENABLE = axisConfig.useDest
    val M_DEST_WIDTH = intfAxisConfig.destWidth
    val S_DEST_WIDTH = slaveIntfAxisConfig.destWidth
    val USER_ENABLE = axisConfig.useUser
    val USER_WIDTH = intfAxisConfig.userWidth
    val TDEST_ROUTE = tdestRoute
  }
  val modName = "axis_demux"
  setBlackBoxName(modName)

  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()

    val enable = in Bool()
    val drop = in Bool()
    val select = in UInt(log2Up(numMasterPorts) bits)
  }

  val s_axis = new DriveMissing(Axi4Stream(slaveAxisConfig), slave(Axi4Stream(slaveIntfAxisConfig)))
  val m_axis = new DriveMissingVec(Axi4Stream(axisConfig), master(Axi4Stream(intfAxisConfig)), numMasterPorts)

  mapCurrentClockDomain(io.clk, io.rst)
  noIoPrefix()

  addPrePopTask { () =>
    renameAxi4StreamIO(alwaysAddT = true)
  }

  // tie off drop and select when TDEST_ROUTE is enabled
  Component.current.parent.rework {
    if (tdestRoute) {
      io.drop := False
      io.select := 0
    }
  }

  addRTLPath(axisRTLFile(modName))
}
