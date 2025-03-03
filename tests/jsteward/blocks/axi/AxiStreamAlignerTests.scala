package jsteward.blocks.axi

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba4.axis.Axi4StreamConfig
import spinal.lib.bus.amba4.axis.sim.{Axi4StreamMaster, Axi4StreamSlave}
import jsteward.blocks.DutSimFunSuite
import spinal.lib.BytesRicher

import scala.collection.mutable
import scala.util.Random

class AxiStreamAlignerTests extends DutSimFunSuite[AxiStreamAligner] {
  val axisConfig = Axi4StreamConfig(dataWidth = 64, useKeep = true, useLast = true)
  val dut = SimConfig.withConfig(SpinalConfig())
    .withFstWave
    .withVerilator
    .allOptimisation
    .compile(AxiStreamAligner(axisConfig))

  def setup(dut: AxiStreamAligner) = {
    SimTimeout(40000)
    dut.clockDomain.forkStimulus(period = 4)

    val packetIn = Axi4StreamMaster(dut.io.input, dut.clockDomain,
      nullSegmentProb = 1,
      maxNullSegment = 63, // first beat should have at least one beat
      nullSegmentOnlyAtBeginning = true,
    )
    val packetOut = Axi4StreamSlave(dut.io.output, dut.clockDomain)

    (packetIn, packetOut)
  }

  test("randomized gap at front") { dut =>
    val (packetIn, packetOut) = setup(dut)

    val checkDataQueue = mutable.Queue[List[Byte]]()

    fork {
      while (true) {
        val pld = packetOut.recv()

        if (checkDataQueue.isEmpty) {
          assert(pld.isEmpty, s"not expecting data on output stream but got ${pld.bytesToHex}")
        } else {
          check(checkDataQueue.dequeue(), pld)
        }
      }
    }

    (0 until 50) foreach { _ =>
      val toSend = Random.nextBytes(Random.between(4, 256)).toList
      checkDataQueue.enqueue(toSend)
      packetIn.send(toSend)
    }
  }
}
