package jsteward.blocks.axi

import spinal.core._
import spinal.lib.bus.amba4.axis.{Axi4Stream, Axi4StreamConfig}
import spinal.lib.io.InOutVecToBits
import spinal.lib._

class AxiStreamArbMux(
                     axisConfig: Axi4StreamConfig,
                     numSlavePorts: Int = 4,
                     updateTid: Boolean = false,
                     arbRoundRobin: Boolean = true,
                     arbLsbHighPriority: Boolean = true,
                     ) extends BlackBox {
  assert(clockDomain.config.resetKind == SYNC, "verilog-axi requires")

  val masterAxisConfig = if (updateTid) axisConfig.copy(idWidth = axisConfig.idWidth + log2Up(numSlavePorts)) else axisConfig
  val intfAxisConfig = mapToIntf(axisConfig)
  val masterIntfAxisConfig = mapToIntf(masterAxisConfig)
  val generic = new Generic {
    val S_COUNT =  numSlavePorts
    val DATA_WIDTH = axisConfig.dataWidth * 8
    val KEEP_ENABLE = axisConfig.useKeep
    val KEEP_WIDTH = axisConfig.dataWidth
    val LAST_ENABLE = axisConfig.useLast
    val ID_ENABLE = axisConfig.useId
    val S_ID_WIDTH = intfAxisConfig.idWidth
    val M_ID_WIDTH = intfAxisConfig.idWidth + log2Up(numSlavePorts)
    val DEST_ENABLE = axisConfig.useDest
    val DEST_WIDTH = intfAxisConfig.destWidth
    val USER_ENABLE = axisConfig.useUser
    val USER_WIDTH = intfAxisConfig.userWidth
    val UPDATE_TID = updateTid
    val ARB_TYPE_ROUND_ROBIN = arbRoundRobin
    val ARB_LSB_HIGH_PRIORITY = arbLsbHighPriority
  }
  val modName = "axis_arb_mux"
  setBlackBoxName(modName)

  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()

    val m_axis = master(Axi4Stream(masterIntfAxisConfig))
  }

  val s_axis = new InOutVecToBits(slave(Axi4Stream(intfAxisConfig)), numSlavePorts)

  lazy val slavePorts = Seq.tabulate(numSlavePorts)(s_axis(_).toSpinal(axisConfig))
  lazy val masterPort = io.m_axis.toSpinal(masterAxisConfig)

  mapCurrentClockDomain(io.clk, io.rst)
  noIoPrefix()

  addPrePopTask { () =>
    renameAxi4StreamIO(alwaysAddT = true)
  }

  addRTLPath(axisRTLFile(modName))
  addRTLPath(axiRTLFile("arbiter"))
  addRTLPath(axiRTLFile("priority_encoder"))
}