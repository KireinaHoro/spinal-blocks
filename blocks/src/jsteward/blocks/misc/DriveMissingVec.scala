package jsteward.blocks.misc

import spinal.core._

import scala.collection.mutable

/**
 * Combined idea of [[DriveMissing]] to drive disabled blackbox ports, and [[spinal.lib.io.InOutVecToBits]] to synthesize
 * flattened bit vector for vector of ports.
 *
 * @param dataType type used in SpinalHDL, without disabled ports
 * @param expected type required by blackbox interface, with disabled ports; used to generate IO
 * @param driveMethod how to drive the missing input port; defaults to assignDontCare
 * @param count number of replications
 * @tparam T SpinalHDL data type
 */
class DriveMissingVec[T <: MultiData](dataType: => T, expected: => T, count: Int, driveMethod: DriveMethod = AssignDontCare) extends Area with collection.IndexedSeq[T] {
  def apply(i: Int) = accesses(i)
  def length = accesses.length

  val mapping = mutable.LinkedHashMap[BaseType, Bits]()
  val template = expected.setName("")
  val accesses = Component.current.parent.rework {
    Vec.fill(count)(dataType.setAsDirectionLess())
  }
  val keys = template.flatten zip template.flattenLocalName

  // generate Bits array from template
  for ((e, _) <- keys) {
    val array = Bits(widthOf(e) * count bits).setCompositeName(this, e.getName())
    e.getDirection match {
      case `in` => in(array)
      case `out` => out(array)
    }
    mapping(e) = array
    e.removeStatement()
  }

  // generate access points in parent component
  Component.current.parent.rework {
    val accessKeys = accesses.map { ab =>
      ab.flatten zip ab.flattenLocalName
    }
    for (i <- keys.indices) {
      val (kb, kn) = keys(i)
      val key = mapping(kb)
      val slices = key.subdivideIn(count slices)
      for (sliceId <- 0 until count) {
        val e: Bits = slices(sliceId)
        val ab = accessKeys(sliceId).find(_._2 == kn).map(_._1)
        (key.getDirection, ab) match {
          case (`in`, Some(a)) => e := a.asBits
          case (`in`, None) => driveMethod.drive(e, accesses(sliceId))
          case (`out`, Some(a)) => a.assignFromBits(e)
          case _ =>
        }
      }
    }
  }
}