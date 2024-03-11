package jsteward.blocks.eci

import spinal.core.{IntToBuilder, RangePimper}

package object sim {
  implicit class RangeRicher(r: Range) {
    def toMask = ((1 << (r.high - r.low + 1)) - 1) << r.low
  }

  implicit class BigIntRicher(i: BigInt) {
    def assignToRange(range: Range, v: BigInt): BigInt = {
      val mask = range.toMask
      (i & mask) | ((v & mask) << range.low)
    }

    def apply(range: Range): BigInt = {
      val mask = range.toMask
      ((i & mask) >> range.low)
    }

    def apply(idx: Int): Boolean = {
      (i & (1 << idx)) != 0
    }
  }

  def aliasCachelineIndex(cli: BigInt): BigInt = {
    BigInt(0)
      .assignToRange(32 downto 13, cli(32 downto 13))
      .assignToRange(12 downto 8, cli(12 downto 8) ^ cli(17 downto 13))
      .assignToRange(7 downto 5, cli(7 downto 5) ^ cli(20 downto 18))
      .assignToRange(4 downto 3, cli(4 downto 3) ^ cli(17 downto 16) ^ cli(6 downto 5))
      .assignToRange(2 downto 0, cli(2 downto 0) ^ cli(15 downto 13) ^ cli(7 downto 5))
  }

  def unaliasCachelineIndex(aliased_cli: BigInt): BigInt = {
    BigInt(0)
      .assignToRange(32 downto 13, aliased_cli(32 downto 13))
      .assignToRange(12 downto 8, aliased_cli(12 downto 8) ^ aliased_cli(17 downto 13))
      .assignToRange(7 downto 5, aliased_cli(7 downto 5) ^ aliased_cli(20 downto 18))
      .assignToRange(4 downto 3, aliased_cli(4 downto 3) ^ aliased_cli(19 downto 18) ^ aliased_cli(17 downto 16) ^ aliased_cli(6 downto 5))
      .assignToRange(2 downto 0, aliased_cli(2 downto 0) ^ aliased_cli(20 downto 18) ^ aliased_cli(15 downto 13) ^ aliased_cli(7 downto 5))
  }

  def aliasAddress(addr: BigInt): BigInt = aliasCachelineIndex(addr(39 downto 7)) << 7

  def unaliasAddress(addr: BigInt): BigInt = unaliasCachelineIndex(addr(39 downto 7)) << 7
}