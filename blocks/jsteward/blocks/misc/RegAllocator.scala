package jsteward.blocks.misc

import spinal.core._
import spinal.lib.bus.regif.AccessType

import scala.collection.mutable

/// FIXME: we should port this to RegIf (one day...)

trait RegBlockReadBack {
  def apply(name: String, subName: String = ""): BigInt
}

trait RegBlockAlloc {
  def apply(name: String, subName: String = "", readSensitive: Boolean = false, attr: AccessType = AccessType.RW): BigInt
  def block(name: String, subName: String = "", count: Int, readSensitive: Boolean = false, attr: AccessType = AccessType.RW): Seq[BigInt]
}

class RegAllocatorFactory {
  private case class RegDesc(baseOffset: BigInt, size: BigInt, count: Int, attr: AccessType) {
    def addr(blockBase: BigInt, idx: Int = 0) = {
      assert(idx < count, s"trying to access reg idx $idx, larger than total of $count instances")
      val ret = baseOffset + blockBase + size * idx
      // println(f"returning $ret%#x for baseOffset $baseOffset%#x blockBase $blockBase%#x size $size replications $count idx $idx")
      ret
    }
  }

  private type BlockMap = mutable.LinkedHashMap[String, RegDesc]

  // read bus width: special alignment for read-sensitive registers; they are allocated at the back of the block
  case class RegBlock(blockName: String, blockLen: BigInt, defaultSize: BigInt, readBusWidth: BigInt) {
    private[RegAllocatorFactory] val blockMap = new BlockMap
    private[RegAllocatorFactory] val allocatedBases = mutable.LinkedHashMap[Int, BigInt]()

    // read back allocated block
    private var addrOffset: BigInt = 0
    private var readSensitiveAddrOffset: BigInt = blockLen

    trait FullAlloc {
      def apply(name: String, subName: String = "", idx: Int = 0, count: Int = 1, size: BigInt = defaultSize, readSensitive: Boolean = false, attr: AccessType = AccessType.RW): BigInt
      def toGeneric = new RegBlockAlloc {
        def apply(name: String, subName: String, readSensitive: Boolean, attr: AccessType): BigInt = FullAlloc.this.apply(name, subName, readSensitive = readSensitive, attr = attr)
        def block(name: String, subName: String, count: Int, readSensitive: Boolean, attr: AccessType) =
          for (i <- 0 until count) yield FullAlloc.this.apply(name, subName, i, count, readSensitive = readSensitive, attr = attr)
      }
    }

    def readBack(blockIdx: Int): RegBlockReadBack = {
      assert(allocatedBases.isDefinedAt(blockIdx), s"trying to read back non-existent block index $blockIdx on block $blockName")
      val base = allocatedBases(blockIdx)

      (name: String, subName: String) => {
        blockMap(genKey(name, subName)).addr(base)
      }
    }

    def alloc(base: BigInt, blockIdx: Int): FullAlloc = {
      assert(!allocatedBases.exists(_._2 == base), f"base addr $base%#x already allocated for $blockName!")
      assert(!allocatedBases.isDefinedAt(blockIdx), f"block index $blockIdx already allocated for $blockName!")
      allocatedBases.update(blockIdx, base)

      (name: String, subName: String, idx: Int, count: Int, size: BigInt, readSensitive: Boolean, attr: AccessType) => {
        val key = genKey(name, subName)
        if (allocatedBases.size > 1) {
          // this is a block recall (e.g. multiple instances of the same plugin)
          // ensure definition matches with existing
          val reg = blockMap(key)
          assert(reg.size == size, "tried to recall register as different size")
          assert(reg.count == count, "tried to recall register block with different count")
          reg.addr(base, idx)
        } else {
          // this is a new block
          // check if we allocated the RegDesc already

          // add one register (single) or all regs in block (replicated)
          def pushReg(): BigInt = {
            val r = if (!readSensitive) {
              val out = addrOffset
              addrOffset += size * count
              assert(addrOffset <= blockLen, f"register alloc overflow block length [$base%#x - ${base + blockLen}%#x]")
              out
            } else {
              val out = readSensitiveAddrOffset - readBusWidth * count
              readSensitiveAddrOffset -= readBusWidth * count
              assert(readSensitiveAddrOffset >= 0, f"register alloc underflow special align")
              out
            }
            assert(addrOffset <= readSensitiveAddrOffset, "regular reg and special reg overlap")
            r
          }

          blockMap.get(key) match {
            case None =>
              // create new RegDesc
              blockMap += key -> RegDesc(pushReg(), size, count, attr)
            case Some(desc) =>
              // we should expand the RegDesc by exactly one
              assert(desc.count == count, "tries to read reg with different replication count")
              assert(desc.attr == attr, s"tries to change access type from ${desc.attr} to $attr")
          }

          blockMap(key).addr(base, idx)
        }
      }
    }
  }

  // blocks with the same name should share the same RegBlock map
  private type GlobalMap = mutable.LinkedHashMap[String, RegBlock]

  private val blocks = new GlobalMap

  trait RegAllocWalker[T] {
    def apply(blockName: String, blockIdx: Int, blockBase: BigInt, regName: String, regDesc: RegDesc): T
  }

  def walkMappings[T](walker: RegAllocWalker[T]): Seq[T] = {
    blocks.flatMap { case (blockName, block) =>
      block.allocatedBases.values.zipWithIndex.flatMap { case (base, idx) =>
        block.blockMap.map { case (name, desc) =>
          walker(blockName, idx, base, name, desc)
        }
      }
    }.toSeq
  }

  def dumpAll(): Unit = {
    println("Dumping global register map:")
    walkMappings { case (blockName, blockIdx, base, name, desc) =>
      if (desc.count == 1) {
        // single register
        println(f"[$blockName#$blockIdx]\t${desc.addr(base)}%#x\t: $name (${desc.size} bytes, ${desc.attr})")
      } else {
        0 until desc.count foreach { i =>
          println(f"[$blockName#$blockIdx]\t${desc.addr(base, i)}%#x\t: $name #$i (${desc.size} bytes, ${desc.attr})")
        }
      }
    }
  }

  implicit class StringRich(s: String) {
    def toCMacroName: String = "[A-Z]|\\d+".r.replaceAllIn(s, { m =>
      "_" + m.group(0)
    }).toUpperCase.replace(':', '_')

    def toCName = toCMacroName.toLowerCase
  }

  val typeToMackerelDefMap = mutable.Map[Class[_], String]()
  def addMackerelEpilogue[T](ty: Class[T], defs: String): Unit = {
    typeToMackerelDefMap += (ty -> defs)
  }
  def writeMackerel(variant: String, outPath: os.Path): Unit = {
    // TODO: allow specifying name and description (instead of hard-coding PIONIC)

    val regDefs = walkMappings[String] { case (blockName, idx, base, name, desc) =>
      val rn = s"${blockName}_${idx}_${name.toCName}"
      val ra = desc.attr.toString.toLowerCase
      val addr = desc.addr(base)
      val dsc = s"$name @ $blockName #$idx"
      if (desc.count == 1) {
        f"register $rn $ra addr(base, $addr%#x) \"$dsc\" type(uint${desc.size * 8});"
      } else {
        f"regarray $rn $ra addr(base, $addr%#x) [${desc.count}] \"$dsc\" type(uint${desc.size * 8});"
      }
    }

    os.remove(outPath)
    os.write(outPath, s"""
        |/*
        | * pionic_$variant.dev: register description of the $variant Enzian NIC.
        | *
        | * Describes registers exposed over the CSR interface as well as datatypes of
        | * various descriptors in memory.
        | */
        |device pionic_$variant lsbfirst (addr base) "Enzian NIC over $variant" {
        |${regDefs.mkString("\n")}
        |${typeToMackerelDefMap.values.mkString("\n")}
        |};
        |""".stripMargin)
  }

  def writeHeader(prefix: String, outPath: os.Path): Unit = {
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

  def apply(blockName: String, blockIdx: Int = 0)(base: BigInt, blockLen: BigInt, defaultSize: BigInt)(readBusWidth: BigInt = defaultSize) = {
    val block = blocks.getOrElseUpdate(blockName, RegBlock(blockName, blockLen, defaultSize, readBusWidth))
    assert(block.blockLen == blockLen, "existing block definition mismatch!")
    assert(block.defaultSize == defaultSize, "existing block definition mismatch!")
    // TODO: check overlap between blocks

    block.alloc(base, blockIdx)
  }

  def clear(): Unit = blocks.clear()
}