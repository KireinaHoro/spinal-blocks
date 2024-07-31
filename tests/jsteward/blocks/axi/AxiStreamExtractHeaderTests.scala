package jsteward.blocks.axi

import jsteward.blocks.{DutSimFunSuite, hexToBytesBE}
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba4.axis._
import spinal.lib.bus.amba4.axis.sim._
import spinal.lib.sim._
import spinal.lib._

import scala.util.Random
import scala.collection.mutable

// test for Ethernet header on 64B datapaths
// TODO: also test for longer headers on shorter datapaths
class AxiStreamExtractHeaderTests extends DutSimFunSuite[AxiStreamExtractHeader] {
  val axisConfig = Axi4StreamConfig(dataWidth = 64, useKeep = true, useLast = true)

  val dut = SimConfig.withConfig(SpinalConfig())
    .withFstWave
    .withVerilator
    .allOptimisation
    .compile(AxiStreamExtractHeader(axisConfig, 14)) // 14B ethernet header

  def setup(dut: AxiStreamExtractHeader, mon: Bits => Unit, randomizeKeep: Boolean) = {
    SimTimeout(40000)
    dut.clockDomain.forkStimulus(period = 4)

    val packetIn = Axi4StreamMaster(dut.io.input, dut.clockDomain,
      nullSegmentProb = if (randomizeKeep) 0.5 else 0,
      maxNullSegment = 16)
    val packetOut = Axi4StreamSlave(dut.io.output, dut.clockDomain)

    StreamReadyRandomizer(dut.io.header, dut.clockDomain)
    StreamMonitor(dut.io.header, dut.clockDomain)(mon)

    (packetIn, packetOut)
  }

  // val sampleEthernetPayload = hexToBytesBE("4500004c7d1c400064119b827f0000017f000001d8bdd5550038fe4b66a6af460000000000000002234511110000000100000001000000000000000000000000000000000000007b0000002d")

  def testEthernetHeader(randomizeKeep: Boolean, iterations: Int, dut: AxiStreamExtractHeader) = {
    val hdrsExpected = mutable.Queue[List[Byte]]()
    val payloadsReceived = mutable.Queue[List[Byte]]()

    val (packetIn, packetOut) = setup(dut, { hdr =>
      val expected = hdrsExpected.dequeue()
      println(s"popped header ${expected.bytesToHex}")
      check(expected, hdr.toBytes.toList)
    }, randomizeKeep)

    fork {
      while (true) {
        payloadsReceived.enqueue(packetOut.recv())
      }
    }

    (0 until iterations) foreach { _ =>
      val macAddrs = Random.nextBytes(12).toList
      println(s"pushing mac addrs ${macAddrs.bytesToHex}")
      val ethernetHdr = macAddrs ++ hexToBytesBE("0800")
      val payloadLen = Random.nextInt(512)
      val payload = Random.nextBytes(payloadLen).toList
      val pkt = ethernetHdr ++ payload

      hdrsExpected.enqueue(ethernetHdr)
      packetIn.send(pkt)

      waitUntil(payloadsReceived.nonEmpty)
      check(payload, payloadsReceived.dequeue())
    }
  }

  test("normal operation") { dut =>
    testEthernetHeader(randomizeKeep = false, 50, dut)
  }

  test("randomize TKEEP") { dut =>
    testEthernetHeader(randomizeKeep = true, 50, dut)
  }

  test("early termination") { dut =>
  }
}
