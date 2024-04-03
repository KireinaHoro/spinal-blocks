package jsteward.blocks.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._

object AxiCdc {
  def apply(push: Axi4, pushClock: ClockDomain, popClock: ClockDomain, depth: Int = 2) = new Composite(push, "axiCdc") {
    val pop = Axi4(push.config) addTag ClockDomainTag(popClock)

    SimpleAsyncFifo(push.ar, pop.ar, depth, pushClock, popClock)
    SimpleAsyncFifo(push.aw, pop.aw, depth, pushClock, popClock)
    SimpleAsyncFifo(push.w, pop.w, depth, pushClock, popClock)

    SimpleAsyncFifo(pop.r, push.r, depth, popClock, pushClock)
    SimpleAsyncFifo(pop.b, push.b, depth, popClock, pushClock)
  }.pop
}
