package jsteward.blocks.misc

import spinal.core._

/** Specify how to drive a disabled signal in [[DriveMissing]] and [[DriveMissingVec]] */
sealed trait DriveMethod {
  def drive(e: BaseType, b: MultiData): Unit = {
    this match {
      case AssignDontCare => e.assignDontCare()
      case AssignZero => e := U(0)
      case TieToMember(f: (MultiData => BaseType)) => e := f(b)
    }
  }
}
case object AssignDontCare extends DriveMethod
case object AssignZero extends DriveMethod
/**
 * Tie to a specific member in the parent bundle.  Be careful: if T does not match the actual type of the parent,
 * a [[ClassCastException]] will be raised.
 */
case class TieToMember[T <: MultiData](func: T => BaseType) extends DriveMethod

/**
 * Used to drive inpout ports of blackboxes that are missing in SpinalHDL.  Some blackboxes disable
 * specific ports (e.g. TLAST in AXI-Stream) but the port still exists in RTL.  This tool avoids leaving
 * these disabled ports dangling.
 *
 * @param dataType type used in SpinalHDL, without disabled ports
 * @param expected type required by blackbox interface, with disabled ports; used to generate IO
 * @param driveMethod how to drive the missing input port; defaults to assignDontCare
 * @tparam T SpinalHDL data type
 */
class DriveMissing[T <: MultiData](dataType: => T, expected: => T, driveMethod: DriveMethod = AssignDontCare) extends Area {
  def get: T = accesses

  val accesses = Component.current.parent.rework {
    dataType.setAsDirectionLess()
  }
  val ports = expected.setCompositeName(this)
  // drive missing signals
  Component.current.parent.rework {
    ports.flatten zip ports.flattenLocalName foreach { case (e, en) =>
      accesses.flattenLocalName.indexOf(en) match {
        case idx if idx >= 0 =>
          val a = accesses.flatten(idx)
          e.getDirection match {
            case `in` => e := a
            case `out` => a := e
          }
        case _ if e.isInput => driveMethod.drive(e, accesses)
        case _ =>
      }
    }
  }
}

object DriveMissing {
  implicit def memberImplicit[T <: MultiData](h: DriveMissing[T]): T = h.get
}
