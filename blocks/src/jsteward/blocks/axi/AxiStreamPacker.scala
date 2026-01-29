package jsteward.blocks.axi

import jsteward.blocks.misc._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis.Axi4Stream._
import spinal.lib.bus.amba4.axis._

case class AxiStreamPacker(
                          axisConfig: Axi4StreamConfig,
                          optLowPower: Boolean = true,
                          ) extends BlackBox {
  assert(axisConfig.useLast, "last must be defined to use packer")
  assert(axisConfig.useKeep, "keep must be defined to use packer")
  assert(!axisConfig.useUser, "user not supported yet in packer")
  assert(!axisConfig.useId, "user not supported yet in packer")
  assert(!axisConfig.useDest, "user not supported yet in packer")

  // strb is always present; copy TKEEP in rework if useStrb is low
  val intfAxisConfig = axisConfig.copy(useStrb = true)
  val generic = new Generic {
    val C_AXIS_DATA_WIDTH = axisConfig.dataWidth * 8
    val OPT_LOWPOWER = optLowPower
  }
  val modName = "axispacker"
  setBlackBoxName(modName)

  val io = new Bundle {
    val S_AXI_ACLK = in Bool()
    val S_AXI_ARESETN = in Bool()
  }

  val s_axis = new DriveMissing(Axi4Stream(axisConfig), slave(Axi4Stream(intfAxisConfig)),
    driveMethod = if (axisConfig.useStrb) AssignDontCare else TieToMember[Axi4Stream](_.keep)
  )
  val m_axis = new DriveMissing(Axi4Stream(axisConfig), master(Axi4Stream(intfAxisConfig)))

  mapCurrentClockDomain(io.S_AXI_ACLK, io.S_AXI_ARESETN, resetActiveLevel = LOW)
  noIoPrefix()

  addPrePopTask { () =>
    renameAxi4StreamIO(alwaysAddT = true, allCaps = true)
  }

  addRTLPath(zipCpuRTLFile(modName))
  addRTLPath(zipCpuRTLFile("skidbuffer"))
}
