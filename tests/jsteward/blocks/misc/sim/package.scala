package jsteward.blocks.misc

import spinal.core.{IntToBuilder, RangePimper, log2Up}
import spinal.lib.{IntRicher, LiteralRicher, LongRicher}

package object sim {
  implicit class BigIntRicher(i: BigInt) {
    def assignToRange(range: Range, v: BigInt): BigInt = {
      val mask = range.mask
      (i &~ mask) | ((v << range.low) & mask)
    }

    def apply(range: Range): BigInt = {
      val mask = range.mask
      (i & mask) >> range.low
    }

    def apply(idx: Int): Boolean = {
      (i & (1 << idx)) != 0
    }

    implicit def toBigInt: BigInt = i
  }

  /** Build a [[BigInt]] by assigning to segments starting from the LSB, one at a time. */
  class BigIntBuilder {
    private var currOffset = 0
    private var currValue = BigInt(0)
    /** push one segment with a given width */
    def push(width: Int, v: BigInt, skip: Int = 0): this.type = {
      assert(log2Up(v) <= width, s"value $v cannot fit into $width bits")
      currOffset += skip

      currValue = currValue.assignToRange(width+currOffset-1 downto currOffset, v)
      currOffset += width
      this
    }
    /** push one segment to pad the whole [[BigInt]] to a given width */
    def pushTo(totalWidth: Int, v: BigInt, skip: Int = 0): this.type = {
      currOffset += skip

      val w = totalWidth - currOffset
      assert(log2Up(v) <= w, s"value $v cannot fit into $w bits")
      assert(totalWidth > currOffset, s"must have at least one bit to push!  current width: $currOffset, tried to grow to $totalWidth bits")

      currValue = currValue.assignToRange(totalWidth-1 downto currOffset, v)
      currOffset = totalWidth
      this
    }
    /** return the assembled value */
    def toBigInt: BigInt = currValue
  }

  /** Decompose a [[BigInt]] by extracting fixed segments from it, starting from the LSB. */
  class BigIntParser(v: BigInt) {
    private var currOffset = 0
    /** pop one of given width */
    def pop(width: Int, skip: Int = 0): BigInt = {
      currOffset += skip

      val ret = v(currOffset+width-1 downto currOffset)
      currOffset += width
      ret
    }
    /** pop one up to given total width */
    def popTo(totalWidth: Int, skip: Int = 0): BigInt = {
      currOffset += skip

      assert(totalWidth >= currOffset, s"trying to pop to a width that's already extracted")
      val ret = v(totalWidth-1 downto currOffset)
      currOffset = totalWidth
      ret
    }
  }

  // choose element randomly from iterator
  // https://stackoverflow.com/a/34819417/5520728
  def choose[A](it: Iterator[A], r: util.Random): A =
    it.zipWithIndex.reduceLeft((x, y) =>
      if (r.nextInt(y._2 + 1) == 0) y else x)._1

  def isSorted[T](s: T*)(implicit ord: Ordering[T]): Boolean = s.toList match {
    case Nil => true
    case _ :: Nil => true
    case x :: xs => ord.lteq(x, xs.head) && isSorted(xs: _*)
  }

  def hexToBytesBE(bytes: String): List[Byte] = bytes.grouped(2).map(Integer.parseInt(_, 16).toByte).toList
  def intToBytesBE[T](a: T, len: Int = 4)(implicit tr: T => LiteralRicher): List[Byte] = a.toBigInt.toByteArray.reverse.padTo(len, 0.toByte).reverse.toList
}
