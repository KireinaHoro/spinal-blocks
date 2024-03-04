package jsteward.blocks

import spinal.core._
import spinal.lib._
package object misc {
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
      val ret = b.clone.asInstanceOf[T]
      locator(ret) := f(locator(b))
      ret.assignUnassignedByName(b)
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
