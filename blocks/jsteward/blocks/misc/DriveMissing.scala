package jsteward.blocks.misc

import spinal.core._

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
class DriveMissing[T <: MultiData](dataType: => T, expected: => T, driveMethod: BaseType => Unit = _.assignDontCare()) extends Area {
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
        case _ if e.isInput => driveMethod(e)
        case _ =>
      }
    }
  }
}

object DriveMissing {
  implicit def memberImplicit[T <: MultiData](h: DriveMissing[T]): T = h.get
}
