package jsteward.blocks.axi

import jsteward.blocks.misc._
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
  val bytesPerWord = if (axisConfig.useKeep) 1 else axisConfig.dataWidth
  val depthWords = depthBytes / bytesPerWord
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
  val modName = "axis_async_fifo"
  setBlackBoxName(modName)

  val io = new Bundle {
    val s_clk = in Bool()
    val s_rst = in Bool()

    val m_clk = in Bool()
    val m_rst = in Bool()

    val s_pause = new FifoPause
    val m_pause = new FifoPause

    val s_status = out(new FifoStatus(depthWords))
    val m_status = out(new FifoStatus(depthWords))
  }

  val s_axis = new DriveMissing(Axi4Stream(axisConfig), slave(Axi4Stream(intfAxisConfig)))
  val m_axis = new DriveMissing(Axi4Stream(axisConfig), master(Axi4Stream(intfAxisConfig)))

  mapClockDomain(clockSlave, io.s_clk, io.s_rst)
  Seq(s_axis.get, io.s_pause, io.s_status) foreach { bus =>
    bus.addTag(ClockDomainTag(clockSlave))
  }

  mapClockDomain(clockMaster, io.m_clk, io.m_rst)
  Seq(m_axis.get, io.m_pause, io.m_status) foreach { bus =>
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

/** Simple FIFO wrapped around the AXI-Stream FIFO for easy CDC. */
case class SimpleAsyncFifo[T <: Data](payloadType: HardType[T],
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
                                     )(
                                       clockSlave: ClockDomain,
                                       clockMaster: ClockDomain,
                                     ) extends Component {
  // FIXME: we round up payload to byte boundary due to SpinalHDL AXI-Stream enforcing byte size
  val actualWidth = roundUp(payloadType.getBitsWidth, 8).toInt
  val axisConfig = Axi4StreamConfig(dataWidth = actualWidth / 8)
  val fifo = AxiStreamAsyncFifo(axisConfig, depthWords, ramPipeline, outputFifoEnable, frameFifo, userBadFrameValue,
    userBadFrameMask, dropBadFrame, dropWhenFull, markWhenFull, pauseEnable)(dropOversizeFrame, framePause)(clockSlave,
    clockMaster)

  val slavePort = slave(Stream(payloadType))
  val masterPort = master(Stream(payloadType))

  slavePort.translateInto(fifo.s_axis) { case (fp, mp) =>
    fp.data := mp.asBits.resized
  }
  masterPort.translateFrom(fifo.m_axis) { case (mp, fp) =>
    mp.assignFromBits(fp.data.resized)
  }
}

object SimpleAsyncFifo {
  def apply[T <: Data](push: Stream[T], pop: Stream[T], depth: Int, pushClock: ClockDomain, popClock: ClockDomain): SimpleAsyncFifo[T] = {
    val actualWidth = roundUp(push.payloadType.getBitsWidth, 8).toInt
    val fifo = SimpleAsyncFifo(push.payloadType, depth * actualWidth / 8)()(pushClock, popClock)
    fifo.slavePort << push
    fifo.masterPort >> pop

    fifo
  }
}