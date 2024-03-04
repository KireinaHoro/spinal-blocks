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
    def remapAddr(f: UInt => UInt): Axi4 = new Composite(axi, "remapped") {
      val ret = Axi4(axi.config)
      ret.aw << axi.aw.mapPayloadElement(_.addr)(f)
      ret.w << axi.w
      ret.b >> axi.b
      ret.ar << axi.ar.mapPayloadElement(_.addr)(f)
      ret.r >> axi.r
    }.ret

    def resize(newWidth: Int): Axi4 = new Composite(axi, "resized") {
      val ret = if (newWidth == axi.config.dataWidth) {
        axi
      } else {
        val adapter = new AxiAdapter(axi.config, newWidth) setCompositeName (axi, "adapter")
        axi >> adapter.slavePort
        adapter.masterPort
      }
    }.ret

    def toSpinal(config: Axi4Config): Axi4 = new Composite(axi, "toSpinal") {
      val ret = Axi4(config)
      val masterChannels: Seq[Axi4 => lib.Stream[_ <: Bundle]] = Seq(_.ar, _.aw, _.w)
      val slaveChannels: Seq[Axi4 => lib.Stream[_ <: Bundle]] = Seq(_.r, _.b)
      val driverChannels = if (axi.isMasterInterface) masterChannels else slaveChannels
      val loadChannels = if (axi.isMasterInterface) slaveChannels else masterChannels
      driverChannels.foreach { c =>
        c(ret).translateFrom(c(axi))(_ <<? _)
      }
      loadChannels.foreach { c =>
        c(axi).translateFrom(c(ret))(_ <<? _)
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
    def toSpinal(config: Axi4StreamConfig): Axi4Stream = {
      val ret = Axi4Stream(config).setCompositeName(axis, "toSpinal", true)
      if (axis.isMasterInterface) {
        ret.translateFrom(axis)(_ <<? _)
      } else {
        axis.translateFrom(ret)(_ <<? _)
      }
      ret
    }

    def frameLength: Flow[UInt] = {
      val clockDomain = axis.getTag(classOf[ClockDomainTag]).map(_.clockDomain).getOrElse(ClockDomain.current)
      val monitor = clockDomain(AxiStreamFrameLen(axis.config))
      monitor.driveFrom(axis)
      monitor.io.frame_len
    }
  }

  implicit class RichAxi4StreamCustom[T <: Data](axis: Axi4StreamCustom[T]) {
    def toSpinal(config: Axi4StreamCustomConfig[T]): Axi4StreamCustom[T] = {
      val ret = Axi4StreamCustom(config).setCompositeName(axis, "toSpinal", true)
      if (axis.isMasterInterface) {
        ret.translateFrom(axis)(_ <<? _)
      } else {
        axis.translateFrom(ret)(_ <<? _)
      }
      ret
    }
  }
}
