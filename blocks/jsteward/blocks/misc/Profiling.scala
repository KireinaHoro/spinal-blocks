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

  def fromBits(bits: Bits, allowResize: Boolean = false) = {
    val ret = clone
    ret.assignFromBits(if (allowResize) bits.resize(this.getBitsWidth) else bits)
    ret
  }
}

case class Profiler(keys: NamedType[UInt]*)(collectTimestamps: Boolean, parent: Profiler = null) {
  val allKeys = (if (parent != null) parent.keys else Nil) ++ keys
  val timestamps = Timestamps(allKeys)

  def regInit(ts: Timestamps) = {
    allKeys foreach { key =>
      ts(key) init 0
    }
  }

  def fillSlot(ts: Timestamps, key: NamedType[UInt], cond: Bool, register: Boolean = true)(implicit clock: CycleClock): Timestamps = {
    assert(keys.contains(key), s"key ${key.getName} not found in the profiler")

    if (register) {
      val capture = RegNextWhen(clock.bits, cond) init 0
      ts(key) := capture.resized
    } else {
      when(cond) {
        ts(key) := clock.bits.resized
      } otherwise {
        ts(key) := 0
      }
    }

    ts
  }

  /** Fill multiple slots */
  def fillSlots(ts: Timestamps, keycond: (NamedType[UInt], Bool)*)(implicit clock: CycleClock): Timestamps = {
    if (!collectTimestamps) {
      return ts
    }

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
    val stage = parent.timestamps.fromBits(upstream)
    parent.keys foreach { pk =>
      downstream(pk) := stage(pk)
    }
  }
}