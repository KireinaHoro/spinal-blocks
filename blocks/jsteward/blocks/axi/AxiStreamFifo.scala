package jsteward.blocks.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis.{Axi4Stream, Axi4StreamConfig}

import scala.language.postfixOps

case class AxiStreamFifo(
                          axisConfig: Axi4StreamConfig,
                          depthWords: Int = 4096,
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
                        ) extends BlackBox {
  assert(clockDomain.config.resetKind == SYNC, "verilog-axis requires synchronous reset")

  val intfAxisConfig = mapToIntf(axisConfig)
  val generic = new Generic {
    val DEPTH = depthWords
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
  val modName = "axis_fifo"
  setBlackBoxName(modName)

  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()

    val s_axis = slave(Axi4Stream(intfAxisConfig))
    val m_axis = master(Axi4Stream(intfAxisConfig))

    val pause = new FifoPause

    val status = out(new FifoStatus(depthWords))
  }

  // TODO: reimplement as rework to generate proper access points like in `InOutVecToBits`
  lazy val slavePort = io.s_axis.toSpinal(axisConfig)
  lazy val masterPort = io.m_axis.toSpinal(axisConfig)

  mapCurrentClockDomain(io.clk, io.rst)
  noIoPrefix()
  addPrePopTask { () =>
    renameAxi4StreamIO(alwaysAddT = true)
  }

  addRTLPath(axisRTLFile(modName))

  Component.current.parent.rework {
    if (!pauseEnable) {
      io.pause.req := False
    }
  }
}
