package jsteward.blocks.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis._
import spinal.lib.bus.amba4.axis.Axi4Stream.Axi4Stream

case class AxiStreamFrameLen(axisConfig: Axi4StreamConfig, lenWidth: Int = 16) extends BlackBox {
  assert(clockDomain.config.resetKind == SYNC, "verilog-axis requires synchronous reset")

  val keepWidth = axisConfig.dataWidth // BYTES
  val generic = new Generic {
    val DATA_WIDTH = axisConfig.dataWidth * 8
    val KEEP_ENABLE = axisConfig.useKeep
    val KEEP_WIDTH = keepWidth
    val LEN_WIDTH = lenWidth
  }
  val modName = "axis_frame_len"
  setBlackBoxName(modName)

  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()

    val monitor_axis = new Bundle {
      val tkeep = in UInt (keepWidth bits)
      val tvalid = in Bool()
      val tready = in Bool()
      val tlast = in Bool()
    }

    val frame_len = master(Flow(UInt(lenWidth bits)))
  }

  def driveFrom(axis: Axi4Stream): Unit = {
    io.monitor_axis.tkeep := axis.payload.keep.asUInt
    io.monitor_axis.tvalid := axis.valid
    io.monitor_axis.tready := axis.ready
    io.monitor_axis.tlast := axis.payload.last
  }

  mapCurrentClockDomain(io.clk, io.rst)
  noIoPrefix()
  addPrePopTask { () =>
    Component.current.getAllIo.find(_.getName == "frame_len_payload").get.setName("frame_len")
  }

  addRTLPath(axisRTLFile(modName))
}
