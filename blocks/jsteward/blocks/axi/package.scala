package jsteward.blocks

import axi.Axi4StreamCustom.Axi4StreamCustom
import jsteward.blocks.misc._
import spinal.core._
import spinal.lib
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axilite.AxiLite4
import spinal.lib.bus.amba4.axis.{Axi4Stream, Axi4StreamConfig}
import spinal.lib.bus.amba4.axis.Axi4Stream.Axi4Stream

import scala.collection.mutable
import scala.language.postfixOps

package object axi {
  class FifoPause extends Bundle {
    val req = in Bool()
    val ack = out Bool()
  }

  class FifoStatus(depthWords: Int) extends Bundle {
    val depth = UInt(log2Up(depthWords) + 1 bits)
    val depth_commit = UInt(log2Up(depthWords) + 1 bits)
    val overflow = Bool()
    val bad_frame = Bool()
    val good_frame = Bool()
  }

  def renameAxi4IO(c: Component = Component.current): Unit = {
    c.getAllIo.foreach { bt =>
      val pattern = "^([sm]_axil?.*)(aw|w|b|ar|r)_(?:payload_)?([^_]+)$".r
      for (pm <- pattern.findFirstMatchIn(bt.getName)) {
        bt.setName(pm.group(1) + pm.group(2) + pm.group(3))
      }
    }
  }

  def renameAxi4StreamIO(c: Component = Component.current, alwaysAddT: Boolean = false): Unit = {
    c.getAllIo.foreach { bt =>
      val pattern = "^((?:s|m|mon)_axis.*?)(?:payload_)*([^_]+)$".r
      for (pm <- pattern.findFirstMatchIn(bt.getName)) {
        val busName = pm.group(1)
        val signalName = if (busName.endsWith("data_") || alwaysAddT) s"t${pm.group(2)}" else pm.group(2)
        bt.setName(busName + signalName)
      }
    }
  }

  def axiRTLFile(name: String) = getClass.getResource(s"/verilog-axi/rtl/$name.v").getPath

  def axisRTLFile(name: String) = getClass.getResource(s"/verilog-axis/rtl/$name.v").getPath

  implicit class RichAxiLite4(axil: AxiLite4) {
    def cdc(pushClock: ClockDomain, popClock: ClockDomain) = new Composite(axil, "cdc") {
      val c = new AxiLiteCdc(axil.config, pushClock, popClock)
      c.io.s_axil << axil
    }.c.io.m_axil

    def resize(newWidth: Int): AxiLite4 = new Composite(axil, "resized") {
      val ret = if (newWidth == axil.config.dataWidth) {
        axil
      } else {
        val adapter = new AxiLiteAdapter(axil.config, newWidth) setCompositeName (axil, "adapter")
        axil >> adapter.io.s_axil
        adapter.io.m_axil
      }
    }.ret
  }

  implicit class RichAxi4(axi: Axi4) {
    def fullPipe(): Axi4 = axi.pipelined(
      aw = StreamPipe.FULL,
      ar = StreamPipe.FULL,
      w = StreamPipe.FULL,
      r = StreamPipe.FULL,
      b = StreamPipe.FULL,
    )

    def remapAddr(f: UInt => UInt): Axi4 = new Composite(axi, "remapped") {
      val ret = Axi4(axi.config)
      ret.aw << axi.aw.mapPayloadElement(_.addr)(f)
      ret.w << axi.w
      ret.b >> axi.b
      ret.ar << axi.ar.mapPayloadElement(_.addr)(f)
      ret.r >> axi.r
    }.ret

    def cdc(pushClock: ClockDomain, popClock: ClockDomain, depth: Int = 2) = AxiCdc(axi, pushClock, popClock, depth)

    def resize(newWidth: Int): Axi4 = new Composite(axi, "resized") {
      val ret = if (newWidth == axi.config.dataWidth) {
        axi
      } else {
        val adapter = new AxiAdapter(axi.config, newWidth) setCompositeName (axi, "adapter")
        axi >> adapter.s_axi
        adapter.m_axi.accesses
      }
    }.ret
  }

  // convert a spinal lib Axi4StreamConfig to verilog-axis format
  def mapToIntf(config: Axi4StreamConfig): Axi4StreamConfig =
    config.copy(
      useLast = true,
      useKeep = true,
      useId = true,
      idWidth = if (config.useId) config.idWidth else 1,
      useDest = true,
      destWidth = if (config.useDest) config.destWidth else 1,
      useUser = true,
      userWidth = if (config.useUser) config.userWidth else 1,
    )

  implicit class RichAxi4Stream(axis: Axi4Stream) {
    def frameLength: Flow[UInt] = new Composite(axis, "frameLength") {
      val clockDomain = axis.getTag(classOf[ClockDomainTag]).map(_.clockDomain).getOrElse(ClockDomain.current)
      val monitor = clockDomain(AxiStreamFrameLen(axis.config))
      monitor.driveFrom(axis)
      val ret = monitor.io.frame_len
    }.ret

    def throwFrameWhen(cond: Flow[Bool]) = new Composite(axis, "throwFrameWhen") {
      val dropper = AxiStreamDropFrame(axis.config)
      dropper.io.input << axis
      dropper.io.drop << cond

      val ret = dropper.io.output
    }.ret

    def takeFrameWhen(cond: Flow[Bool]) = throwFrameWhen(cond.map(!_))
  }
}
