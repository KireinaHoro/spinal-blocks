package jsteward.blocks

import axi.Axi4StreamCustom.Axi4StreamCustom
import spinal.core._
import spinal.lib
import spinal.lib._
import spinal.lib.bus.amba4.axi._
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

  def renameAxi4IO: Unit = {
    Component.current.getAllIo.foreach { bt =>
      val pattern = "^([sm]_axi.*)(aw|w|b|ar|r)_(?:payload_)?([^_]+)$".r
      for (pm <- pattern.findFirstMatchIn(bt.getName)) {
        bt.setName(pm.group(1) + pm.group(2) + pm.group(3))
      }
    }
  }

  def renameAxi4StreamIO(alwaysAddT: Boolean = false): Unit = {
    Component.current.getAllIo.foreach { bt =>
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

  implicit class RichBundle(b: Bundle) {
    def <<?(that: Bundle): Unit = {
      b.assignSomeByName(that)
      b.assignDontCareToUnasigned()
    }

    def >>?(that: Bundle): Unit = {
      that <<? b
    }
  }

  implicit class RichAxi4(axi: Axi4) {
    def resize(newWidth: Int): Axi4 = {
      val adapter = new AxiAdapter(axi.config, newWidth)
      axi >> adapter.slavePort
      adapter.masterPort
    }

    def toSpinal(config: Axi4Config): Axi4 = {
      val ret = Axi4(config).setCompositeName(axi, "toSpinal", true)
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
      ret
    }
  }

  // convert a spinal lib Axi4StreamConfig to verilog-axis format
  def mapToIntf(config: Axi4StreamConfig): Axi4StreamConfig =
    config.copy(
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
