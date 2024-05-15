package jsteward.blocks.misc.sim

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.collection.mutable

object StreamMonitorThreadful {
  def apply[T <: Data](stream: Stream[T], clockDomain: ClockDomain)(callback: T => Unit) = new StreamMonitorThreadful(stream, clockDomain).addCallback(callback)
}

class StreamMonitorThreadful[T <: Data](stream: Stream[T], clockDomain: ClockDomain) {
  val callbacks = mutable.ArrayBuffer[T => Unit]()
  def addCallback(callback: T => Unit): this.type = {
    callbacks += callback
    this
  }

  val validProxy = stream.valid.simProxy()
  val readyProxy = stream.ready.simProxy()

  fork {
    while (true) {
      clockDomain.waitActiveEdge()

      fork {
        // for every clock cycle, launch a thread to check for fire condition
        // callback could take multiple clock cycles to finish; execute them in a separate thread
        if (validProxy.toBoolean && readyProxy.toBoolean && clockDomain.isSamplingEnable) {
          callbacks.foreach(_(stream.payload))
        }
      }
    }
  }
}
