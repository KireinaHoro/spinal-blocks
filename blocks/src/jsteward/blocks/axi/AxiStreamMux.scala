package jsteward.blocks.axi

import jsteward.blocks.misc.{DriveMissing, DriveMissingVec}
import spinal.core._
import spinal.lib.bus.amba4.axis.{Axi4Stream, Axi4StreamConfig}
import spinal.lib._

import scala.language.postfixOps


class AxiStreamMux(
                  axisConfig: Axi4StreamConfig,
                  numSlavePorts: Int = 4,
                  ) extends BlackBox {
  assert(clockDomain.config.resetKind == SYNC, "verilog-axi requires")
  assert(numSlavePorts > 1, "unable to generate mux with only one input")

  val intfAxisConfig = mapToIntf(axisConfig)
  val generic = new Generic {
    val S_COUNT = numSlavePorts
    val DATA_WIDTH = axisConfig.dataWidth * 8
    val KEEP_ENABLE = axisConfig.useKeep
    val KEEP_WIDTH = axisConfig.dataWidth
    val ID_ENABLE = axisConfig.useId
    val ID_WIDTH = intfAxisConfig.idWidth
    val DEST_ENABLE = axisConfig.useDest
    val DEST_WIDTH = intfAxisConfig.destWidth
    val USER_ENABLE = axisConfig.useUser
    val USER_WIDTH = intfAxisConfig.userWidth
  }
  val modName = "axis_mux"
  setBlackBoxName(modName)

  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()

    val enable = in Bool()
    val select = in UInt(log2Up(numSlavePorts) bits)
  }

  val s_axis = new DriveMissingVec(Axi4Stream(axisConfig), slave(Axi4Stream(intfAxisConfig)), numSlavePorts)
  val m_axis = new DriveMissing(Axi4Stream(axisConfig), master(Axi4Stream(intfAxisConfig)))

  mapCurrentClockDomain(io.clk, io.rst)
  noIoPrefix()

  addPrePopTask { () =>
    renameAxi4StreamIO(alwaysAddT = true)
  }

  addRTLPath(axisRTLFile(modName))
}
