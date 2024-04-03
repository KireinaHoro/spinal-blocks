package jsteward.blocks

import spinal.core._
import spinal.lib._

package object misc {
  def checkParam[T](v: T)(candidates: T*): Unit = {
    if (!candidates.contains(v)) SpinalError(s"parameter $v not in list of valid options $candidates")
  }

  implicit class RichBundle[T <: Bundle](b: T) {
    def <<?(that: Bundle): Unit = {
      b.assignSomeByName(that)
      b.assignDontCareToUnasigned()
    }

    def >>?(that: Bundle): Unit = {
      that <<? b
    }
  }

  implicit class RichMultiData[T <: MultiData](b: T) {
    def mapElement[E <: Data](locator: T => E)(f: E => E): T = {
      val ret = b.clone.asInstanceOf[T].allowOverride()
      ret := b
      locator(ret) := f(locator(b))
      // ret.assignUnassignedByName(b) // doesn't work for Union!
      ret
    }
  }

  implicit class RichStreamMultiData[T <: MultiData](s: Stream[T]) {
    def mapPayloadElement[E <: Data](locator: T => E)(f: E => E): Stream[T] = {
      val ret = s.clone
      ret.translateFrom(s) { case (r, ss) =>
        r := ss.mapElement(locator)(f)
      }
    }
  }
}
