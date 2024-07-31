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

  test("abnormal packets") { implicit dut =>
    val headerOnly = hexToBytesBE("abcdef1234560102030405060800")
    val incompleteHeader = hexToBytesBE("112233445566aabbccdd") // 4B short

    var headerReceived = false
    val (packetIn, packetOut) = setup(dut, { hdr =>
      // we should only receive the second one
      assert(!headerReceived, "received more than one header!")

      check(headerOnly, hdr.toBytes.toList)
      headerReceived = true
    }, true)

    fork {
      while (true) {
        val payload = packetOut.recv()
        assert(payload.isEmpty, "should not receive data on the stream")
      }
    }

    packetIn.send(incompleteHeader)
    sleepCycles(50)
    assert(dut.io.statistics.incompleteHeader.toBigInt == 1, "incomplete header counter did not increase")
    assert(dut.io.statistics.headerOnly.toBigInt == 0, "header only counter increased")

    packetIn.send(headerOnly)
    sleepCycles(50)
    assert(dut.io.statistics.incompleteHeader.toBigInt == 1, "incomplete header changed")
    assert(dut.io.statistics.headerOnly.toBigInt == 1, "header only counter did not increase")
  }
}
