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

case class Profiler(keys: NamedType[UInt]*)(collectTimestamps: Boolean) extends Area {
  val timestamps = Reg(Timestamps(keys))

  def regInit(ts: Timestamps) = {
    keys foreach { key =>
      ts(key) init 0
    }
  }

  def fillSlot(key: NamedType[UInt], cond: Bool)(implicit clock: CycleClock) = {
    assert(keys.contains(key), s"key ${key.getName} not found in the profiler")

    when(cond) {
      timestamps(key) := clock.bits.resized
    }
  }

  /** Fill multiple slots */
  def fillSlots(keycond: (NamedType[UInt], Bool)*)(implicit clock: CycleClock) = {
    if (collectTimestamps) {
      keycond foreach { case (key, cond) =>
        fillSlot(key, cond)
      }
    }
  }
}