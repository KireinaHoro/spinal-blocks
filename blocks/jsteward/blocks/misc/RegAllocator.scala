package jsteward.blocks.misc

import spinal.core._

import scala.collection.mutable

trait RegBlockReadBack {
  def apply(name: String, subName: String = ""): BigInt
}

class RegAllocatorFactory {
  private case class RegDesc(baseOffset: BigInt, size: BigInt) {
    def addr(blockBase: BigInt) = baseOffset + blockBase
  }

  private type BlockMap = mutable.LinkedHashMap[String, RegDesc]

  case class RegBlock(blockName: String, blockLen: BigInt, defaultSize: BigInt) {
    private[RegAllocatorFactory] val blockMap = new BlockMap
    private[RegAllocatorFactory] val allocatedBases = mutable.LinkedHashMap[Int, BigInt]()

    // read back allocated block
    private var addrOffset: BigInt = 0

    trait RegBlockAlloc {
      def apply(name: String, subName: String = "", size: BigInt = defaultSize): BigInt
    }

    def readBack(blockIdx: Int): RegBlockReadBack = {
      assert(allocatedBases.isDefinedAt(blockIdx), s"trying to read back non-existent block index $blockIdx on block $blockName")
      val base = allocatedBases(blockIdx)

      (name: String, subName: String) => {
        blockMap(genKey(name, subName)).addr(base)
      }
    }

    def alloc(base: BigInt, blockIdx: Int): RegBlockAlloc = {
      assert(!allocatedBases.exists(_._2 == base), f"base addr $base%#x already allocated for $blockName!")
      assert(!allocatedBases.isDefinedAt(blockIdx), f"block index $blockIdx already allocated for $blockName!")
      allocatedBases.update(blockIdx, base)

      (name: String, subName: String, size: BigInt) => {
        if (allocatedBases.size > 1) {
          // this is a recall; ensure definition matches with existing
          val reg = blockMap(genKey(name, subName))
          assert(reg.size == size)
          reg.addr(base)
        } else {
          // this is a new block
          val currOffset = addrOffset
          addrOffset += size
          assert(addrOffset <= blockLen, f"register alloc overflow block length [$base%#x - ${base + blockLen}%#x]")
          blockMap.update(s"${genKey(name, subName)}", RegDesc(currOffset, size))
          currOffset + base
        }
      }
    }
  }

  // blocks with the same name should share the same RegBlock map
  private type GlobalMap = mutable.LinkedHashMap[String, RegBlock]

  private val blocks = new GlobalMap

  def dumpAll(): Unit = {
    println("Dumping global register map:")
    blocks.foreach { case (blockName, block) =>
      block.allocatedBases.values.foreach { base =>
        println(f"==============")
        block.blockMap.foreach { case (name, desc) =>
          println(f"[$blockName@$base%#x] ${desc.addr(base)}%#x\t: $name (${desc.size} bytes)")
        }
      }
    }
  }

  // TODO: emit JSON for driver generator
  def writeHeader(prefix: String, outPath: os.Path): Unit = {
    implicit class StringRich(s: String) {
      def toCMacroName: String = "[A-Z]|\\d+".r.replaceAllIn(s, { m =>
        "_" + m.group(0)
      }).toUpperCase.replace(':', '_')

      def toCName = toCMacroName.toLowerCase
    }
    val prefixCN = prefix.toCName
    val prefixCMN = prefix.toCMacroName

    val defLines = blocks.flatMap { case (blockName, block) =>
      val bname = blockName.toCMacroName
      if (block.allocatedBases.size == 1) {
        block.blockMap.flatMap { case (name, desc) =>
          val rname = name.toCMacroName
          val base = block.allocatedBases.values.head
          Seq(
            f"#define ${prefixCMN}_${bname}_$rname ${desc.addr(base)}%#x",
            f"#define ${prefixCMN}_${bname}_${rname}_SIZE ${desc.size}%#x",
          )
        }
      } else {
        val arrayName = s"__${prefixCN}_${blockName.toCName}_bases"
        Seq(
          f"static uint64_t $arrayName[] __attribute__((unused)) = {",
        ) ++ block.allocatedBases.values.map { base =>
          f"  $base%#x,"
        } ++ Seq("};") ++ block.blockMap.flatMap { case (name, desc) =>
          val rname = name.toCMacroName
          Seq(
            f"#define ${prefixCMN}_${bname}_$rname(blockIdx) (${desc.baseOffset}%#x + $arrayName[blockIdx])",
            f"#define ${prefixCMN}_${bname}_${rname}_SIZE ${desc.size}%#x",
          )
        }
      } :+ ""
    }

    os.remove(outPath)
    os.write(outPath,
      defLines.mkString(
        s"""|#ifndef __${prefixCMN}_REGS_H__
            |#define __${prefixCMN}_REGS_H__
            |
            |""".stripMargin,
        "\n",
        s"""|
            |#endif // __${prefixCMN}_REGS_H__
            |""".stripMargin))
  }

  private def genKey(name: String, subName: String) = {
    val delim = if (subName.nonEmpty) ":" else ""
    s"$name$delim$subName"
  }

  def readBack(blockName: String, blockIdx: Int = 0) = {
    assert(blocks.contains(blockName), s"attempting to read back block $blockName that does not exist")

    val block = blocks(blockName)
    block.readBack(blockIdx)
  }

  def apply(blockName: String, blockIdx: Int = 0)(base: BigInt, blockLen: BigInt, defaultSize: BigInt) = {
    val block = blocks.getOrElseUpdate(blockName, RegBlock(blockName, blockLen, defaultSize))
    assert(block.blockLen == blockLen, "existing block definition mismatch!")
    assert(block.defaultSize == defaultSize, "existing block definition mismatch!")
    // TODO: check overlap between blocks

    block.alloc(base, blockIdx)
  }

  def clear(): Unit = blocks.clear()
}