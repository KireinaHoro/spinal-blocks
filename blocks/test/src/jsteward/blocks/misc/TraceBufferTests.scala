package jsteward.blocks.misc

import spinal.core._
import spinal.core.sim._
import spinal.lib.sim._

import scala.collection.mutable

import jsteward.blocks.DutSimFunSuite

class TraceBufferTests extends DutSimFunSuite[TraceBuffer[UInt]] {
  val dut = SimConfig
    .withConfig(SpinalConfig())
    .withFstWave
    .withVerilator
    .allOptimisation
    .compile(TraceBuffer(UInt(32 bits), numInputs = 6, numSlots = 512))

  def setup(implicit dut: TraceBuffer[UInt]) = {
    SimTimeout(40000)
    dut.clockDomain.forkStimulus(period = 4)

    val eventQs = Array.fill(dut.numInputs)(mutable.Queue[Long]())

    dut.traceIn.zipWithIndex.foreach { case (ip, idx) =>
      FlowDriver(ip, dut.clockDomain) { tip =>
        if (eventQs(idx).nonEmpty) {
          tip #= eventQs(idx).dequeue()
          true
        } else {
          false
        }
      }
    }

    sleepCycles(10)

    eventQs
  }

  test("events from last port") { implicit dut =>
    val evQs = setup
    val toCheck = mutable.HashSet[(Long, Int)]()
    val total = 256

    0 until total foreach { idx =>
      val v = simRandom.nextLong(1L << 32)
      val port = idx % 6

      toCheck.add((v, port))
      println(f"Pushing event $v%#x to port $port")

      evQs(port).enqueue(v)
    }

    waitUntil(evQs.map(_.isEmpty).reduce(_ && _))
    sleepCycles(20)

    // read out what's in the buffer
    dut.dump #= true
    dut.clockDomain.waitActiveEdge(2)
    0 until total foreach { idx =>
      val ev = dut.traceOut.event.toLong
      val src = dut.traceOut.src.toInt
      val ts = dut.traceOut.ts.toBigInt

      println(s"Received ev=$ev from port $src (ts=$ts)")
      assert(toCheck.remove((ev, src)))
      dut.clockDomain.waitActiveEdge()
    }
  }
}
