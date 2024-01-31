package jsteward.blocks.axi

import spinal.core._
import spinal.lib.bus.amba4.axis._
import spinal.lib._

import scala.language.postfixOps

case class AxiStreamAsyncFifo(
                               axisConfig: Axi4StreamConfig,
                               depthBytes: Int = 4096,
                               ramPipeline: Boolean = true,
                               outputFifoEnable: Boolean = false,
                               frameFifo: Boolean = false,
                               userBadFrameValue: Int = 1,
                               userBadFrameMask: Int = 1,
                               dropBadFrame: Boolean = false,
                               dropWhenFull: Boolean = false,
                               markWhenFull: Boolean = false,
                               pauseEnable: Boolean = false,
                             )(
                               dropOversizeFrame: Boolean = frameFifo,
                               framePause: Boolean = frameFifo,
                             )(
                               clockSlave: ClockDomain,
                               clockMaster: ClockDomain,
                             ) extends BlackBox {
  assert(clockDomain.config.resetKind == SYNC, "verilog-axis requires synchronous reset")

  val intfAxisConfig = mapToIntf(axisConfig)
  val generic = new Generic {
    val DEPTH = depthBytes
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
    val RAM_PIPELINE = ramPipeline
    val OUTPUT_FIFO_ENABLE = outputFifoEnable
    val FRAME_FIFO = frameFifo
    val USER_BAD_FRAME_VALUE = userBadFrameValue
    val USER_BAD_FRAME_MASK = userBadFrameMask
    val DROP_OVERSIZE_FRAME = dropOversizeFrame
    val DROP_BAD_FRAME = dropBadFrame
    val DROP_WHEN_FULL = dropWhenFull
    val MARK_WHEN_FULL = markWhenFull
    val PAUSE_ENABLE = pauseEnable
    val FRAME_PAUSE = framePause
  }
  val modName = "axis_async_fifo"
  setBlackBoxName(modName)

  class Pause extends Bundle {
    val req = in Bool()
    val ack = out Bool()
  }

  class Status extends Bundle {
    val depth = UInt(log2Up(depthBytes) bits)
    val depth_commit = UInt(log2Up(depthBytes) bits)
    val overflow = Bool()
    val bad_frame = Bool()
    val good_frame = Bool()
  }

  val io = new Bundle {
    val s_clk = in Bool()
    val s_rst = in Bool()
    val s_axis = slave(Axi4Stream(intfAxisConfig))

    val m_clk = in Bool()
    val m_rst = in Bool()
    val m_axis = master(Axi4Stream(intfAxisConfig))

    val s_pause = new Pause
    val m_pause = new Pause

    val s_status = out(new Status)
    val m_status = out(new Status)
  }

  // TODO: reimplement as rework to generate proper access points like in `InOutVecToBits`
  lazy val slavePort = io.s_axis.toSpinal(axisConfig) addTag ClockDomainTag(clockSlave)
  lazy val masterPort = io.m_axis.toSpinal(axisConfig) addTag ClockDomainTag(clockMaster)

  mapClockDomain(clockSlave, io.s_clk, io.s_rst)
  Seq(io.s_axis, io.s_pause, io.s_status) foreach { bus =>
    bus.addTag(ClockDomainTag(clockSlave))
  }

  mapClockDomain(clockMaster, io.m_clk, io.m_rst)
  Seq(io.m_axis, io.m_pause, io.m_status) foreach { bus =>
    bus.addTag(ClockDomainTag(clockMaster))
  }

  noIoPrefix()
  addPrePopTask { () =>
    renameAxi4StreamIO(alwaysAddT = true)
  }

  addRTLPath(axisRTLFile(modName))

  Component.current.parent.rework {
    if (!pauseEnable) {
      io.m_pause.req := False
      io.s_pause.req := False
    }
  }
}
