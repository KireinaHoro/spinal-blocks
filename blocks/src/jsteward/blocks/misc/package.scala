package jsteward.blocks

import spinal.core._
import spinal.lib.StreamPipe.FULL
import spinal.lib._

import scala.language.postfixOps

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

  implicit class RichData[T <: Data](b: T) {
    def mapElement[E <: Data](locator: T => E)(f: E => E): T = {
      val ret = b.clone.asInstanceOf[T].allowOverride()
      ret := b
      locator(ret) := f(locator(b))
      // ret.assignUnassignedByName(b) // doesn't work for Union!
      ret
    }
  }

  implicit class RichStream[T <: Data](s: Stream[T]) {
    def mapPayloadElement[E <: Data](locator: T => E)(f: E => E): Stream[T] = {
      val ret = s.clone
      ret.translateFrom(s) { case (r, ss) =>
        r := ss.mapElement(locator)(f)
      }
    }

    /** generate a stream that drives the current stream, with a padded payload */
    def padSlave(low: Int, high: Int = 0): Stream[Bits] = {
      val pw = s.payload.getBitsWidth
      val ret = Stream(Bits(pw+low+high bits))
      ret.translateInto(s) { case (orig, padded) =>
        orig.assignFromBits(padded(low+pw-1 downto low))
      }
      ret
    }

    def farPipe(levels: Int = 3): Stream[T] = new Composite(s, "farPipe") {
      assert(levels >= 1)
      val p = s.pipelined(FULL)
      val ret = if (levels == 1) p else p.farPipe(levels - 1)
    }.ret
  }

  object StreamDispatcherWithEnable {
    def apply[T <: Data](
                          input: Stream[T],
                          outputCount: Int,
                          enableMask: Flow[Bits],
                        ): Vec[Stream[T]] = new ImplicitArea[Vec[Stream[T]]] {
      // FIXME: same as OHMasking.roundRobin?
      assert(
        outputCount == enableMask.payload.getWidth,
        "enable mask bit width does not match with output count"
      )
      val select = Reg(UInt(log2Up(outputCount) bits))
      val mask = Reg(enableMask.payload.clone)

      // reset select when mask changes
      // FIXME: can this happen when a request is ongoing?
      when(enableMask.valid) {
        select := CountTrailingZeroes(enableMask.payload).resized
        mask := enableMask.payload
      }

      val doubleMask = mask ## mask
      val shiftedMask = doubleMask >> (select + 1)
      val inc = CountTrailingZeroes(shiftedMask.resize(outputCount)) + 1
      when(input.fire) {
        select := select + inc.resized
      }
      val implicitValue = StreamDemux(input, select, outputCount)
    }.setCompositeName(input, "streamDispatch", true)
  }
}
