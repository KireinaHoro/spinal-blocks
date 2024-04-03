package jsteward.blocks.mem.xpm

import spinal.core._
import spinal.core.internals._

class PhaseMemBlackBoxingXpm(policy: MemBlackboxingPolicy) extends PhaseMemBlackBoxingWithPolicy(policy) {
  override def doBlackboxing(topo: MemTopology): String = {
    val mem = topo.mem

    def wrapBool(that: Expression): Bool = that match {
      case that: Bool => that
      case that =>
        val ret = Bool()
        ret.assignFrom(that)
        ret
    }

    def wrapConsumers(oldSource: Expression, newSource: Expression): Unit = {
      super.wrapConsumers(topo, oldSource, newSource)
    }

    def removeMem(): Unit = {
      super.removeMem(mem)
    }

    def wrapWe(we: Bits, port: WritePort) = {
      val _we = we.clone
      if (port.mask != null)
        _we.assignFrom(port.mask)
      else
        _we.setAll()

      when(wrapBool(port.writeEnable)) {
        we := _we
      } otherwise {
        we.clearAll()
      }
    }

    def getReadPort(topo: MemTopology): ReadPort = {
      assert(topo.readsAsync.nonEmpty || topo.readsSync.nonEmpty)
      var rd: ReadPort = null

      for (r <- topo.readsSync) {
        rd = r
      }
      for (r <- topo.readsAsync) {
        rd = r
      }

      rd
    }

    def mapTechnology(mem: Mem[_]) = mem.technology match {
      case `auto` => "auto"
      case `distributedLut` => "distributed"
      case `ramBlock` => "block"
      case _ => SpinalError(s"XPM memory does not support technology: ${mem.technology}")
    }

    def mapRUW(port: ReadPort) = port.readUnderWrite match {
      case `dontCare` | `eitherFirst` => "no_change"
      case `readFirst` => "read_first"
      case `writeFirst` => "write_first"
    }

    if (mem.initialContent != null) {
      return "WIP: can't blackbox ROM or RAM with initial content"
    } else if (topo.portCount == 1 && topo.readWriteSync.size == 1) {
      // single port RAM
      mem.component.rework {
        val port = topo.readWriteSync.head

        val ram = port.clockDomain on new XpmMemorySpRam(
          addrWidthA = port.getAddressWidth,
          readDataWidthA = port.width,
          writeDataWidthA = port.width,
          memorySize = mem.wordCount * mem.width,
          memoryPrimitive = mapTechnology(mem),
          writeModeA = mapRUW(port),
          useWriteMaskA = port.mask != null,
        )

        ram.io.addra.assignFrom(port.address)
        ram.io.dina.assignFrom(port.data)
        wrapConsumers(port, ram.io.douta)
        ram.io.ena := wrapBool(port.chipSelect)
        ram.io.regcea := port.clockDomain.isClockEnableActive
        wrapWe(ram.io.wea, port)

        ram.io.sleep := False

        ram.addTag(new MemBlackboxOf(topo.mem.asInstanceOf[Mem[Data]]))

        ram.setName(mem.getName())
        removeMem()
      }
    } else if (topo.writes.size == 1 && (topo.readsAsync.nonEmpty || topo.readsSync.nonEmpty) && topo.writeReadSameAddressSync.isEmpty && topo.readWriteSync.isEmpty) {
      // simple dual port RAM
      mem.component.rework {
        val wr = topo.writes.head
        val rd = getReadPort(topo)

        val rdDomain = rd.clockDomain(wr.clockDomain)

        val ram = new XpmMemorySDpRam(
          clockDomainA = wr.clockDomain,
          clockDomainB = rdDomain,
          addrWidthB = rd.getAddressWidth,
          readDataWidthB = rd.width,
          addrWidthA = wr.getAddressWidth,
          writeDataWidthA = wr.width,
          memorySize = mem.wordCount * mem.width,
          memoryPrimitive = mapTechnology(mem),
          writeModeB = mapRUW(rd),
          useWriteMaskA = wr.mask != null,
        )

        ram.io.addrb.assignFrom(rd.address)
        wrapConsumers(rd.toExpression, ram.io.doutb)
        ram.io.enb := wrapBool(rd.readEnable)
        ram.io.regceb := rdDomain.isClockEnableActive

        ram.io.addra.assignFrom(wr.address)
        ram.io.dina.assignFrom(wr.data)
        ram.io.ena := wrapBool(wr.writeEnable) && wr.clockDomain.isClockEnableActive
        wrapWe(ram.io.wea, wr)

        // tie off unused
        ram.io.sleep := False
        ram.io.injectsbiterra := False
        ram.io.injectdbiterra := False

        ram.addTag(new MemBlackboxOf(topo.mem.asInstanceOf[Mem[Data]]))

        ram.setName(mem.getName())
        removeMem()
      }
    } else if (topo.portCount == 2 && topo.readWriteSync.size == 1 && (topo.readsSync.nonEmpty || topo.readsAsync.nonEmpty)) {
      // one readwrite, one read
      // true dual port RAM
      mem.component.rework {
        val rw = topo.readWriteSync.head
        val rd = getReadPort(topo)

        val rdDomain = rd.clockDomain(rw.clockDomain)

        val ram = new XpmMemoryTDpRam(
          clockDomainA = rw.clockDomain,
          clockDomainB = rdDomain,
          addrWidthA = rw.getAddressWidth,
          addrWidthB = rd.getAddressWidth,
          readDataWidthA = rw.width,
          writeDataWidthA = rw.width,
          readDataWidthB = rd.width,
          memorySize = mem.wordCount * mem.width,
          memoryPrimitive = mapTechnology(mem),
          writeModeA = mapRUW(rd),
          writeModeB = mapRUW(rw),
          useWriteMaskA = rw.mask != null,
        )

        ram.io.addra.assignFrom(rw.address)
        ram.io.dina.assignFrom(rw.data)
        wrapConsumers(rw.toExpression, ram.io.douta)
        ram.io.ena := wrapBool(rw.readEnable) || wrapBool(rw.writeEnable)
        ram.io.regcea := rw.clockDomain.isClockEnableActive
        wrapWe(ram.io.wea, rw)

        ram.io.addrb.assignFrom(rd.address)
        wrapConsumers(rd.toExpression, ram.io.doutb)
        ram.io.enb := wrapBool(rd.readEnable)
        ram.io.regceb := rdDomain.isClockEnableActive
        // write side of port is unused
        ram.io.web.clearAll()
        ram.io.dinb.assignDontCare()

        // tie off unused
        ram.io.sleep := False
        ram.io.injectsbiterra := False
        ram.io.injectdbiterra := False
        ram.io.injectsbiterrb := False
        ram.io.injectdbiterrb := False

        ram.addTag(new MemBlackboxOf(topo.mem.asInstanceOf[Mem[Data]]))

        ram.setName(mem.getName())
        removeMem()
      }
    } else {
      return "Unblackboxable memory topology"
    }

    null
  }
}
