package jsteward.blocks.misc

import spinal.core.RangePimper

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
}
