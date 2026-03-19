package jsteward.blocks.misc

import spinal.core._
import spinal.core.sim._
import spinal.lib.sim._

import scala.collection.mutable

import jsteward.blocks.DutSimFunSuite

class TraceBufferTests extends DutSimFunSuite[TraceBuffer[UInt]] {
  val dut = SimConfig
    .withConfig(SpinalConfig().includeSimulation)
    .withFstWave
    .withVerilator
    .allOptimisation
    .compile(TraceBuffer(UInt(32 bits), numInputs = 6, numSlots = 512))

  def setup(implicit dut: TraceBuffer[UInt]) = {
    SimTimeout(40000)
    dut.dump #= false

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
      
      if (idx % 16 == 15) {
        // throttle send so we don't lose samples
        println(f"Waiting for events to drain")
        sleepCycles(40)
      }

      evQs(port).enqueue(v)
    }

    waitUntil(evQs.map(_.isEmpty).reduce(_ && _))
    sleepCycles(20)

    // read out what's in the buffer
    dut.dump #= true
    dut.clockDomain.waitActiveEdge(2)

    var samplesLeft = dut.numSlots
    while (toCheck.nonEmpty) {
      assert(samplesLeft > 0)
      samplesLeft -= 1

      assert(!dut.sampleLost.toBoolean)

      val ev = dut.traceOut.event.toLong
      val src = dut.traceOut.src.toInt
      val ts = dut.traceOut.ts.toBigInt

      println(s"Received ev=$ev from port $src (ts=$ts)")
      
      val found = toCheck.remove((ev, src))
      if (!found) {
        println(f"Event $ev%#x from port $src not found!")
        println(s"Events left: ${toCheck.mkString("; ")}")
        fail()
      }

      dut.clockDomain.waitActiveEdge()
    }
    
    // check that no garbage trace is emitted
    // FIXME: there's no good way to check this without including the initialization file path
    //        in the generated verilog, which breaks Verilator's caching
    //while (samplesLeft > 0) {
    //  assert(dut.traceOut.ts.toBigInt == 0)
    //  dut.clockDomain.waitActiveEdge()
    //  samplesLeft -= 1
    //}
  }
}
