package jsteward.blocks.misc

import jsteward.blocks.DutSimFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axi.sim._
import spinal.lib.sim._

import scala.collection.mutable
import scala.util.Random

class TraceBufferDMATests extends DutSimFunSuite[TraceBufferDMA[UInt]] {
  val numInputs = 3
  val bufferSlots = 128
  val payloadWidth = 32
  val timestampWidth = 64
  val lostCountWidth = 16
  val axiDataWidth = 128
  val axiBytes = axiDataWidth / 8
  val axiBufferBase = BigInt(0x1000)
  val axiBufferSize = BigInt(bufferSlots * axiBytes)

  val axiConfig = Axi4Config(
    addressWidth = 16,
    dataWidth = axiDataWidth,
    idWidth = 1,
    useQos = false,
    useRegion = false,
  )

  val dut = SimConfig
    .withConfig(SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC)
    ).includeSimulation)
    .withFstWave
    .withVerilator
    .addSimulatorFlag("-Wno-SELRANGE -Wno-WIDTH -Wno-CASEINCOMPLETE")
    .allOptimisation
    .compile(TraceBufferDMA(
      UInt(payloadWidth bits),
      numInputs = numInputs,
      axiConfig = axiConfig,
      axiBufferBase = axiBufferBase,
      axiBufferSize = axiBufferSize,
      burstFifoSize = 4,
      frameFifoSize = 4,
      timestampWidth = timestampWidth,
      lostCountWidth = lostCountWidth,
      axiMaxBurstLen = 4,
    ))

  case class DecodedFrame(event: Long, src: Int, ts: BigInt, tracesLost: Boolean, lostCount: Long)

  def bits(value: BigInt, offset: Int, width: Int): BigInt =
    (value >> offset) & ((BigInt(1) << width) - 1)

  def decode(word: BigInt): DecodedFrame = {
    val srcWidth = log2Up(numInputs)
    val eventOffset = 0
    val srcOffset = eventOffset + payloadWidth
    val tsOffset = srcOffset + srcWidth
    val lostOffset = tsOffset + timestampWidth
    val lostCountOffset = lostOffset + 1

    DecodedFrame(
      event = bits(word, eventOffset, payloadWidth).toLong,
      src = bits(word, srcOffset, srcWidth).toInt,
      ts = bits(word, tsOffset, timestampWidth),
      tracesLost = bits(word, lostOffset, 1) == 1,
      lostCount = bits(word, lostCountOffset, lostCountWidth).toLong,
    )
  }

  def setup(writeResponseDelay: Int = 0)(implicit dut: TraceBufferDMA[UInt]) = {
    SimTimeout(80000)

    dut.clockDomain.forkStimulus(period = 4)
    val memory = AxiMemorySim(
      dut.axi,
      dut.clockDomain,
      AxiMemorySimConfig(writeResponseDelay = writeResponseDelay),
    )
    memory.start()

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
    (eventQs, memory)
  }

  def readFrames(memory: AxiMemorySim, count: Int): Seq[DecodedFrame] =
    (0 until count).map { idx =>
      decode(memory.memory.readBigInt((axiBufferBase + idx * axiBytes).toLong, axiBytes))
    }

  test("writes events to DRAM") { implicit dut =>
    val (eventQs, memory) = setup()
    val expected = mutable.HashSet[(Long, Int)]()
    val total = 24

    0 until total foreach { idx =>
      val value = Random.nextLong(1L << payloadWidth)
      val port = idx % numInputs
      expected.add((value, port))
      eventQs(port).enqueue(value)

      if (idx % 6 == 5) {
        sleepCycles(20)
      }
    }

    waitUntil(eventQs.forall(_.isEmpty))
    waitUntil(dut.writeSlot.toInt >= total)
    sleepCycles(80)

    assert(!dut.sampleLost.toBoolean)
    assert(!dut.dmaError.toBoolean)

    val frames = readFrames(memory, total)
    frames.foreach { frame =>
      assert(!frame.tracesLost, s"unexpected lost-traces frame: $frame")
      assert(frame.lostCount == 0)
      assert(frame.ts > 0)
      assert(expected.remove((frame.event, frame.src)), s"unexpected trace frame: $frame")
    }
    assert(expected.isEmpty, s"missing expected trace frames: ${expected.mkString("; ")}")
  }

  test("inserts lost-traces placeholder on overflow") { implicit dut =>
    val (eventQs, memory) = setup(writeResponseDelay = 40)
    val total = 96

    0 until total foreach { idx =>
      eventQs(idx % numInputs).enqueue(idx + 1)
    }

    waitUntil(dut.sampleLost.toBoolean)
    waitUntil(eventQs.forall(_.isEmpty))
    waitUntil(dut.writeSlot.toInt >= 32)
    sleepCycles(300)

    val frames = readFrames(memory, 64)
    val lostFrames = frames.filter(_.tracesLost)

    assert(lostFrames.nonEmpty, s"expected at least one lost-traces frame, got ${frames.mkString("; ")}")
    assert(lostFrames.exists(_.lostCount > 0), s"lost-traces frames did not carry a count: ${lostFrames.mkString("; ")}")
    assert(!dut.dmaError.toBoolean)
  }
}
