package jsteward.blocks.misc

import spinal.core._
import spinal.lib.bus.amba4.axis.Axi4Stream.Axi4Stream
import spinal.lib.bus.amba4.axis._

import scala.language.postfixOps

case class CycleClock(width: BitCount) extends Bundle {
  val bits = UInt(width)
}

case class Timestamps(keys: Seq[NamedType[UInt]]) extends HardMap {
  override def clone = Timestamps(keys)

  def dumpFields = elements foreach { case (name, data) =>
    println(s"$name: ${data.getBitsWidth} bits")
  }

  keys.foreach(add)
}

case class Profiler(keys: NamedType[UInt]*)(collectTimestamps: Boolean, parent: Profiler = null) {
  val allKeys = (if (parent != null) parent.keys else Nil) ++ keys
  val timestamps = Timestamps(allKeys)

  def regInit(ts: Timestamps) = {
    allKeys foreach { key =>
      ts(key) init 0
    }
  }

  def augment(axisConfig: Axi4StreamConfig): Axi4StreamConfig = {
    if (collectTimestamps)
      axisConfig.copy(useUser = true, userWidth = timestamps.getBitsWidth)
    else axisConfig
  }

  def fillSlot(ts: Timestamps, key: NamedType[UInt], cond: Bool)(implicit clock: CycleClock): Timestamps = {
    assert(keys.contains(key), s"key ${key.getName} not found in the profiler")

    when(cond) {
      ts(key) := clock.bits.resized
    }

    ts
  }

  /** Fill multiple slots */
  def fillSlots(ts: Timestamps, keycond: (NamedType[UInt], Bool)*)(implicit clock: CycleClock): Timestamps = {
    if (!collectTimestamps) { return ts }

    keycond foreach { case (key, cond) =>
      fillSlot(ts, key, cond)
    }

    ts
  }

  /** Collect existing timestamps from parent profiler */
  def collectInto(upstream: Bits, downstream: Timestamps): Unit = {
    if (!collectTimestamps) {
      downstream.assignDontCare()
      return
    }

    // collect parent keys
    assert(upstream.getWidth == parent.timestamps.getBitsWidth, s"upstream timestamp bits does not match with upstream profiler")
    val stage = parent.timestamps.clone
    stage.assignFromBits(upstream)
    parent.keys foreach { pk =>
      downstream(pk) := stage(pk)
    }
  }

  /** Inject timestamp bundle into a AXI-Stream.  If the stream is already timestamped, add the new timestamp to the map */
  def timestamp(axis: Axi4Stream, key: NamedType[UInt])(implicit clock: CycleClock): Axi4Stream = {
    if (!collectTimestamps) return axis

    // FIXME: we assume that the USER field of the stream is only used to carry timestamps
    val ret = Axi4Stream(this augment axis.config)

    ret << axis
    val stage = timestamps.clone
    stage.assignFromBits(if (axis.config.useUser) axis.user else B(0, timestamps.getBitsWidth bits))
    ret.user.allowOverride()
    ret.user := fillSlots(stage, key -> axis.lastFire).asBits
    ret
  }
}