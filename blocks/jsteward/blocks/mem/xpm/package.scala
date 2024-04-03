package jsteward.blocks.mem

import spinal.core._
import spinal.core.internals._

package object xpm {
  def resetMode(clockDomain: ClockDomain): String = clockDomain.config.resetKind match {
    case `ASYNC` => "ASYNC"
    case `SYNC` => "SYNC"
  }

  trait WritePort {
    def mask: Expression with WidthProvider
    def writeEnable: Expression
  }

  implicit class RWAsWritePort(rw: MemReadWrite) extends WritePort {
    def mask = rw.mask
    def writeEnable = rw.writeEnable
  }

  implicit class WAsWritePort(w: MemWrite) extends WritePort {
    def mask = w.mask
    def writeEnable = w.writeEnable
  }

  trait ReadPort {
    def clockDomain(wrDomain: ClockDomain): ClockDomain
    def getAddressWidth: Int
    def width: Int
    def address: Expression
    def readEnable: Expression
    def readUnderWrite: ReadUnderWritePolicy
    def toExpression: Expression
  }

  implicit class RSAsReadPort(rs: MemReadSync) extends ReadPort {
    def clockDomain(wrDomain: ClockDomain): ClockDomain = rs.clockDomain
    def getAddressWidth: Int = rs.getAddressWidth
    def width: Int = rs.width
    def address: Expression = rs.address
    def readEnable: Expression = rs.readEnable
    def readUnderWrite: ReadUnderWritePolicy = rs.readUnderWrite

    def toExpression: Expression = rs
  }

  implicit class RAAsReadPort(ra: MemReadAsync) extends ReadPort {
    def clockDomain(wrDomain: ClockDomain): ClockDomain = wrDomain
    def getAddressWidth: Int = ra.getAddressWidth
    def width: Int = ra.width
    def address: Expression = ra.address
    def readEnable: Expression = True
    def readUnderWrite: ReadUnderWritePolicy = ra.readUnderWrite
    def toExpression: Expression = ra
  }

  implicit class RWAsReadPort(rw: MemReadWrite) extends ReadPort {
    def clockDomain(wrDomain: ClockDomain): ClockDomain = rw.clockDomain
    def getAddressWidth: Int = rw.getAddressWidth
    def width: Int = rw.width
    def address: Expression = rw.address
    def readEnable: Expression = True
    def readUnderWrite: ReadUnderWritePolicy = rw.readUnderWrite
    def toExpression: Expression = rw
  }
}
