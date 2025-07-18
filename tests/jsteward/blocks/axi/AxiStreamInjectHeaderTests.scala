package jsteward.blocks.axi

import jsteward.blocks.DutSimFunSuite
import jsteward.blocks.misc.sim.hexToBytesBE
import spinal.core._
import spinal.core.sim._
import spinal.lib.BytesRicher
import spinal.lib.bus.amba4.axis._
import spinal.lib.bus.amba4.axis.sim._
import spinal.lib.sim._

import scala.collection.mutable
import scala.util.Random

trait AxiStreamInjectHeaderTestsCommonSetup extends DutSimFunSuite[AxiStreamInjectHeader] {
  def dutGen: AxiStreamInjectHeader

  val axisConfig = Axi4StreamConfig(dataWidth = 64, useKeep = true, useLast = true)
  val dut = SimConfig.withConfig(SpinalConfig())
    .withFstWave
    .withVerilator
    .allOptimisation
    .compile(dutGen)

  def setup(dut: AxiStreamInjectHeader, randomizeKeep: Boolean) = {
    SimTimeout(40000)
    dut.clockDomain.forkStimulus(period = 4)

    val packetIn = Axi4StreamMaster(dut.io.input, dut.clockDomain,
      nullSegmentProb = if (randomizeKeep) 0.5 else 0,
      maxNullSegment = 16,
      nullSegmentOnlyAtBeginning = true,
    )
    packetIn.setFactor(0.9f)
    val packetOut = Axi4StreamSlave(dut.io.output, dut.clockDomain)
    val hdrToSendQueue = mutable.Queue[(List[Byte], List[Byte])]()
    val dataToCheckQueue = mutable.Queue[List[Byte]]()

    StreamDriver(dut.io.header, dut.clockDomain) { h =>
      if (hdrToSendQueue.isEmpty) {
        false
      } else {
        val (hdr, pld) = hdrToSendQueue.dequeue()
        h #= hdr.toArray
        dataToCheckQueue += hdr ++ pld
        true
      }
    }

    fork {
      while (true) {
        val pld = packetOut.recv()
        assert(dataToCheckQueue.nonEmpty, "received output packet when not expecting any")
        check(dataToCheckQueue.dequeue(), pld)
      }
    }

    (packetIn, hdrToSendQueue, dataToCheckQueue)
  }
}

class AxiStreamInjectHeaderEthernetTests extends AxiStreamInjectHeaderTestsCommonSetup {
  def dutGen = AxiStreamInjectHeader(axisConfig, 14)

  def testEthernetHeader(randomizeKeep: Boolean, iterations: Int, dut: AxiStreamInjectHeader) = {
    val (pktIn, hdrQ, dataQ) = setup(dut, randomizeKeep)

    (0 until iterations) foreach { _ =>
      val macAddrs = Random.nextBytes(12).toList
      println(s"pushing mac addrs ${macAddrs.bytesToHex}")
      val ethernetHdr = macAddrs ++ hexToBytesBE("0800")
      val payloadLen = Random.nextInt(512)
      val payload = Random.nextBytes(payloadLen).toList

      hdrQ.enqueue((ethernetHdr, payload))
      pktIn.send(payload)
    }

    waitUntil(hdrQ.isEmpty && dataQ.isEmpty)
  }

  test("normal operation") { dut =>
    testEthernetHeader(randomizeKeep = false, 50, dut)
  }

  test("randomize TKEEP") { dut =>
    testEthernetHeader(randomizeKeep = true, 50, dut)
  }
}
// TODO: test when payload has no gap at the front
// TODO: test longer headers
// TODO: test header size equaling beat size