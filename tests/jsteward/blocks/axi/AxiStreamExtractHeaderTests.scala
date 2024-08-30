package jsteward.blocks.axi

import jsteward.blocks.misc.sim.hexToBytesBE
import jsteward.blocks.DutSimFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba4.axis._
import spinal.lib.bus.amba4.axis.sim._
import spinal.lib.sim._
import spinal.lib._

import scala.util.Random
import scala.collection.mutable

trait AxiStreamExtractHeaderTestsCommonSetup extends DutSimFunSuite[AxiStreamExtractHeader] {
  def dutGen: AxiStreamExtractHeader
  def headerBytes = 14

  val axisConfig = Axi4StreamConfig(dataWidth = 64, useKeep = true, useLast = true)
  val dut = SimConfig.withConfig(SpinalConfig())
    .withFstWave
    .withVerilator
    .allOptimisation
    .compile(dutGen) // 14B ethernet header

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

  def testAbnormalPackets(dut: AxiStreamExtractHeader, expectHeader: Boolean) = {
    implicit val d = dut
    val checkHdrQueue = mutable.Queue[List[Byte]]()
    val checkDataQueue = mutable.Queue[List[Byte]]()

    val (packetIn, packetOut) = setup(dut, { hdr =>
      // we should only receive the second one
      assert(checkHdrQueue.nonEmpty, "expected no more headers!")

      check(checkHdrQueue.dequeue(), hdr.toBytes.toList)
    }, true)

    fork {
      while (true) {
        val payload = packetOut.recv()

        if (checkDataQueue.isEmpty) {
          assert(payload.isEmpty, "should not receive data on the stream")
        } else {
          check(checkDataQueue.dequeue(), payload)
        }
      }
    }

    var incompleteCounter, headerOnlyCounter = 0

    def iteration = {
      val toSend = Random.nextBytes(Random.between(4, headerBytes + 4)).toList
      val hdr = toSend.take(headerBytes).padTo(headerBytes, 0.toByte)

      if (expectHeader || toSend.length >= headerBytes) checkHdrQueue.enqueue(hdr)
      if (toSend.length > headerBytes) checkDataQueue.enqueue(toSend.drop(headerBytes))
      if (toSend.length < headerBytes) incompleteCounter += 1
      if (toSend.length == headerBytes) headerOnlyCounter += 1

      packetIn.send(toSend)
      sleepCycles(50)

      assert(dut.io.statistics.incompleteHeader.toBigInt == incompleteCounter)
      assert(dut.io.statistics.headerOnly.toBigInt == headerOnlyCounter)
    }

    (0 until 50) foreach { _ => iteration }
  }
}

// test for Ethernet header on 64B datapaths
// TODO: also test for longer headers on narrower datapaths
class AxiStreamExtractHeaderTests extends AxiStreamExtractHeaderTestsCommonSetup {
  def dutGen = AxiStreamExtractHeader(axisConfig, headerBytes)

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
    testAbnormalPackets(dut, expectHeader = false)
  }
}
