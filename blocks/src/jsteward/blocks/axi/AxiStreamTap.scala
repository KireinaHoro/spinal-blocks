package jsteward.blocks.axi

import jsteward.blocks.misc.DriveMissing
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis.{Axi4Stream, Axi4StreamConfig}

case class AxiStreamTap(
                       axisConfig: Axi4StreamConfig,
                       userBadFrameValue: Int = 1,
                       userBadFrameMask: Int = 1,
                       ) extends BlackBox {
  assert(clockDomain.config.resetKind == SYNC, "verilog-axis requires synchronous reset")

  val intfAxisConfig = mapToIntf(axisConfig)
  val generic = new Generic {
    val DATA_WIDTH = axisConfig.dataWidth * 8
    val KEEP_ENABLE = axisConfig.useKeep
    val KEEP_WIDTH = axisConfig.dataWidth
    val LAST_ENABLE = axisConfig.useLast
    val ID_ENABLE = axisConfig.useId
    val ID_WIDTH = intfAxisConfig.idWidth
    val DEST_ENABLE = axisConfig.useDest
    val DEST_WIDTH = intfAxisConfig.destWidth
    val USER_ENABLE = axisConfig.useUser
    val USER_WIDTH = intfAxisConfig.userWidth
    val USER_BAD_FRAME_VALUE = userBadFrameValue
    val USER_BAD_FRAME_MASK = userBadFrameMask
  }
  val modName = "axis_tap"
  setBlackBoxName(modName)

  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()
  }

  val tap_axis = new DriveMissing(Axi4Stream(axisConfig), in(Axi4Stream(intfAxisConfig)))
  val m_axis = new DriveMissing(Axi4Stream(axisConfig), master(Axi4Stream(intfAxisConfig)))

  mapCurrentClockDomain(io.clk, io.rst)
  noIoPrefix()
  addPrePopTask { () =>
    renameAxi4StreamIO(alwaysAddT = true)
  }

  addRTLPath(axisRTLFile(modName))
}
