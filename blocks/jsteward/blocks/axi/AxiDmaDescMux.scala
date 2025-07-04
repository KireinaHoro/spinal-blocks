package jsteward.blocks.axi

import jsteward.blocks.misc.{DriveMissing, DriveMissingVec, RichBundle}
import spinal.core._
import spinal.lib._
import spinal.lib.io.InOutVecToBits

class AxiDmaDescMux(
                     clientDmaConfig: AxiDmaConfig,
                     numPorts: Int = 2,
                     arbRoundRobin: Boolean = true,
                     arbLsbHighPriority: Boolean = true,
                   ) extends BlackBox {
  assert(clockDomain.config.resetKind == SYNC, "verilog-axi requires synchronous reset")

  import clientDmaConfig._

  private val mTagWidth: Int = tagWidth + log2Up(numPorts)
  val masterDmaConfig: AxiDmaConfig = clientDmaConfig.copy(tagWidth = mTagWidth)

  val generic = new Generic {
    val PORTS = numPorts
    val AXI_ADDR_WIDTH = axiConfig.addressWidth
    val AXIS_ID_ENABLE = axisConfig.useId
    val AXIS_ID_WIDTH = masterDmaConfig.intfAxisConfig.idWidth
    val AXIS_DEST_ENABLE = axisConfig.useDest
    val AXIS_DEST_WIDTH = masterDmaConfig.intfAxisConfig.destWidth
    val AXIS_USER_ENABLE = axisConfig.useUser
    val AXIS_USER_WIDTH = masterDmaConfig.intfAxisConfig.userWidth
    val LEN_WIDTH = lenWidth
    val S_TAG_WIDTH = tagWidth
    val M_TAG_WIDTH = mTagWidth
    val ARB_TYPE_ROUND_ROBIN = arbRoundRobin
    val ARB_LSB_HIGH_PRIORITY = arbLsbHighPriority
  }
  val modName = "axi_dma_desc_mux"
  setBlackBoxName(modName)

  val clk = in Bool()
  val rst = in Bool()

  val s_axis_desc = new DriveMissingVec(readDescBus, slave(readDescBusIntf), numPorts)
  val m_axis_desc_status = new InOutVecToBits(master(writeDescStatusBus), numPorts)

  // to get all the ports for both read and write
  val m_axis_desc = new DriveMissing(masterDmaConfig.readDescBus, master(masterDmaConfig.readDescBusIntf))
  val s_axis_desc_status = slave(masterDmaConfig.writeDescStatusBus)

  mapCurrentClockDomain(clk, rst)

  addPrePopTask { () =>
    renameAxi4StreamIO()
  }

  addRTLPath(axiRTLFile(modName))
  addRTLPath(axiRTLFile("arbiter"))
  addRTLPath(axiRTLFile("priority_encoder"))

  def connectRead(dma: AxiDma) {
    dma.s_axis_read_desc.translateFrom(m_axis_desc) { case (left, right) =>
      // only allow resizing USER field
      left.elements zip right.elements foreach { case (leftElem, rightElem) =>
        if (leftElem._1 == "user") leftElem._2 := rightElem._2.resized
        else leftElem._2 := rightElem._2
      }
    }
    s_axis_desc_status <<? dma.m_axis_read_desc_status
  }

  def connectWrite(dma: AxiDma) {
    // write desc port does not have id, dest, user
    dma.s_axis_write_desc.translateFrom(m_axis_desc)(_ <<? _)
    s_axis_desc_status << dma.m_axis_write_desc_status
  }
}
