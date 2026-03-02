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

    /** encode a value as bitfield to be used as a segment; handles negative values as well */
    private def encode(width: Int, v: BigInt): BigInt = {
      val mask = (BigInt(1) << width) - 1
      val minAllowed = -(BigInt(1) << (width - 1))
      val truncated = v & mask
      assert(v >= minAllowed && v <= mask,
        f"value $v ($v%x) cannot fit into $width bits (would truncate to $truncated%x)")
      truncated
    }

    /** push one segment with a given width */
    def push(width: Int, v: BigInt, skip: Int = 0): this.type = {
      currOffset += skip

      currValue = currValue.assignToRange(width+currOffset-1 downto currOffset, encode(width, v))
      currOffset += width
      this
    }

    def pushBytes(bytes: List[Byte], skip: Int = 0): this.type = {
      var doSkip = skip
      bytes.foreach { b =>
        push(8, b.toBigInt, doSkip)
        doSkip = 0
      }
      this
    }

    /** push one segment to pad the whole [[BigInt]] to a given width */
    def pushTo(totalWidth: Int, v: BigInt, skip: Int = 0): this.type = {
      currOffset += skip

      val w = totalWidth - currOffset
      assert(totalWidth > currOffset, s"must have at least one bit to push!  current width: $currOffset, tried to grow to $totalWidth bits")

      currValue = currValue.assignToRange(totalWidth-1 downto currOffset, encode(w, v))
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
  def intToBytesBE[T](a: T)(implicit n: Numeric[T]): List[Byte] = {
    val len = a match {
      case _: Short => 2
      case _: Int => 4
      case _: Long => 8
    }
    val al = n.toLong(a)
    val v = if (len == 8) al else {
      al & ((1L << (len * 8)) - 1)
    }
    v.toBigInt.toByteArray
      .reverse                        // so that we can pad zero at MSB
      .padTo(len, 0.toByte).take(len) // so that we have always len bytes
      .reverse.toList                 // restore the original big endianness from toByteArray
  }

  def intToBytesLE[T](a: T)(implicit n: Numeric[T]): List[Byte] = intToBytesBE(a).reverse

  implicit class IntRicherEndianAware[T](v: T)(implicit n: Numeric[T]) {
    def toBytesLE = intToBytesLE(v)
    def toBytesBE = intToBytesBE(v)
  }
}
