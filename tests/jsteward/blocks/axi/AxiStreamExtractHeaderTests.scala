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

  def setup(dut: AxiStreamExtractHeader, randomizeKeep: Boolean) = {
    SimTimeout(40000)
    dut.clockDomain.forkStimulus(period = 4)

    val packetIn = Axi4StreamMaster(dut.io.input, dut.clockDomain,
      nullSegmentProb = if (randomizeKeep) 0.5 else 0,
      maxNullSegment = 16,
      nullSegmentOnlyAtBeginning = true,
    )
    val packetOut = Axi4StreamSlave(dut.io.output, dut.clockDomain)

    // we should always receive header before payload
    val checkHdrQueue = mutable.Queue[(List[Byte], List[Byte])]()
    val checkDataQueue = mutable.Queue[List[Byte]]()

    StreamReadyRandomizer(dut.io.header, dut.clockDomain)
    StreamMonitor(dut.io.header, dut.clockDomain) { hdr =>
      assert(checkHdrQueue.nonEmpty, s"not expecting more headers, got ${hdr.toBytes.toList.bytesToHex}")

      val (expectedHdr, expectedPld) = checkHdrQueue.dequeue()

      check(expectedHdr, hdr.toBytes.toList)

      if (expectedPld.nonEmpty)
        checkDataQueue.enqueue(expectedPld)
    }

    fork {
      while (true) {
        val payload = packetOut.recv()
        if (checkDataQueue.isEmpty) {
          assert(payload.isEmpty, s"not expecting data on the stream, got ${payload.bytesToHex}")
        } else {
          check(checkDataQueue.dequeue(), payload)
        }
      }
    }

    (packetIn, checkHdrQueue, checkDataQueue)
  }

  def testAbnormalPackets(dut: AxiStreamExtractHeader, minHeaderLen: Int) = {
    implicit val d = dut

    val (packetIn, checkHdrQueue, checkDataQueue) = setup(dut, randomizeKeep = true)

    var incompleteCounter, partialCounter, headerOnlyCounter = 0

    def iteration = {
      val toSend = Random.nextBytes(Random.between(4, headerBytes + 4)).toList
      val l = toSend.length
      val hdr = toSend.take(headerBytes).padTo(headerBytes, 0.toByte)

      if (l >= minHeaderLen) checkHdrQueue.enqueue((hdr, toSend.drop(headerBytes)))

      if (l == headerBytes) headerOnlyCounter += 1
      else if (l < headerBytes && l >= minHeaderLen) partialCounter += 1
      else if (l < minHeaderLen) incompleteCounter += 1

      packetIn.send(toSend)
      sleepCycles(50)

      assert(dut.io.statistics.incompleteHeader.toBigInt == incompleteCounter)
      assert(dut.io.statistics.partialHeader.toBigInt == partialCounter)
      assert(dut.io.statistics.headerOnly.toBigInt == headerOnlyCounter)
    }

    (0 until 50) foreach { _ => iteration }

    waitUntil(checkHdrQueue.isEmpty && checkDataQueue.isEmpty)
  }
}

// test for Ethernet header on 64B datapaths
// TODO: also test for longer headers on narrower datapaths
class AxiStreamExtractHeaderTests extends AxiStreamExtractHeaderTestsCommonSetup {
  def dutGen = AxiStreamExtractHeader(axisConfig, headerBytes)()

  def testEthernetHeader(randomizeKeep: Boolean, iterations: Int, dut: AxiStreamExtractHeader) = {
    val (packetIn, hdrsExpected, pldsExpected) = setup(dut, randomizeKeep)

    (0 until iterations) foreach { _ =>
      val macAddrs = Random.nextBytes(12).toList
      println(s"pushing mac addrs ${macAddrs.bytesToHex}")
      val ethernetHdr = macAddrs ++ hexToBytesBE("0800")
      val payloadLen = Random.nextInt(512)
      val payload = Random.nextBytes(payloadLen).toList
      val pkt = ethernetHdr ++ payload

      hdrsExpected.enqueue((ethernetHdr, payload))
      packetIn.send(pkt)
    }

    waitUntil(hdrsExpected.isEmpty && pldsExpected.isEmpty)
  }

  test("normal operation") { dut =>
    testEthernetHeader(randomizeKeep = false, 50, dut)
  }

  test("randomize TKEEP") { dut =>
    testEthernetHeader(randomizeKeep = true, 50, dut)
  }

  test("abnormal packets") { implicit dut =>
    testAbnormalPackets(dut, minHeaderLen = headerBytes)
  }
}
