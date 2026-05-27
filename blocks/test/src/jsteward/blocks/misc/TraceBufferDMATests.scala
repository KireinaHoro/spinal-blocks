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
  val payloadWidth = 78
  val timestampWidth = 48
  val lostCountWidth = 32
  val sourceWidth = log2Up(numInputs + 1)
  val lostSourceId = (BigInt(1) << sourceWidth) - 1
  val sampleWidth = payloadWidth + sourceWidth + timestampWidth
  val axiDataWidth = 512
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
      flushIdleCycles = 64,
    ))

  sealed trait DecodedFrame {
    def ts: BigInt
  }
  case class DecodedSample(event: Long, src: Int, ts: BigInt) extends DecodedFrame
  case class DecodedMarker(lostCount: Long, ts: BigInt) extends DecodedFrame {
    def isBubble: Boolean = lostCount == 0
  }

  def bits(value: BigInt, offset: Int, width: Int): BigInt =
    (value >> offset) & ((BigInt(1) << width) - 1)

  def decode(word: BigInt): DecodedFrame = {
    val payloadOffset = 0
    val srcOffset = payloadOffset + payloadWidth
    val tsOffset = srcOffset + sourceWidth
    val payload = bits(word, payloadOffset, payloadWidth)
    val src = bits(word, srcOffset, sourceWidth).toInt
    val ts = bits(word, tsOffset, timestampWidth)

    if (src == lostSourceId) {
      DecodedMarker(
        lostCount = bits(payload, 0, lostCountWidth).toLong,
        ts = ts,
      )
    } else {
      DecodedSample(
        event = payload.toLong,
        src = src,
        ts = ts,
      )
    }
  }

  def setup(writeResponseDelay: Int = 0)(implicit dut: TraceBufferDMA[UInt]) = {
    SimTimeout(200000)

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
    {
      val bytes = ((count * sampleWidth + 7) / 8).max(axiBytes)
      val allData = memory.memory.readBigInt(axiBufferBase.toLong, bytes)
      (0 until count).map { idx =>
        decode(bits(allData, idx * sampleWidth, sampleWidth))
      }
    }

  def beatsForSamples(count: Int): Int =
    (count * sampleWidth) / axiDataWidth

  test("writes events to DRAM") { implicit dut =>
    val (eventQs, memory) = setup()
    val expected = mutable.HashSet[(Long, Int)]()
    val total = 64

    0 until total foreach { idx =>
      val value = Random.nextLong(1L << 62)
      val port = idx % numInputs
      expected.add((value, port))
      eventQs(port).enqueue(value)

      if (idx % 6 == 5) {
        sleepCycles(20)
      }
    }

    waitUntil(eventQs.forall(_.isEmpty))
    waitUntil(dut.writeSlot.toInt >= beatsForSamples(total))
    sleepCycles(80)

    assert(!dut.sampleLost.toBoolean)
    assert(!dut.dmaError.toBoolean)

    val frames = readFrames(memory, total)
    frames.foreach {
      case frame@DecodedSample(event, src, ts) =>
        assert(ts > 0)
        assert(expected.remove((event, src)), s"unexpected trace frame: $frame")
      case marker: DecodedMarker =>
        fail(s"unexpected marker frame: $marker")
    }
    assert(expected.isEmpty, s"missing expected trace frames: ${expected.mkString("; ")}")
  }

  test("inserts lost-traces placeholder on overflow") { implicit dut =>
    val (eventQs, memory) = setup(writeResponseDelay = 40)
    val total = 256

    0 until total foreach { idx =>
      eventQs(idx % numInputs).enqueue(idx + 1)
    }

    waitUntil(dut.sampleLost.toBoolean)
    waitUntil(eventQs.forall(_.isEmpty))
    waitUntil(dut.writeSlot.toInt >= beatsForSamples(128))
    sleepCycles(300)

    val frames = readFrames(memory, 128)
    val markers = frames.collect { case marker: DecodedMarker => marker }

    assert(markers.nonEmpty, s"expected at least one marker frame, got ${frames.mkString("; ")}")
    assert(markers.exists(_.lostCount > 0), s"marker frames did not carry a lost count: ${markers.mkString("; ")}")
    assert(!dut.dmaError.toBoolean)
  }

  test("flushes partial beat with bubbles after idle timeout") { implicit dut =>
    val (eventQs, memory) = setup()
    val expected = Seq((0x1234L, 0), (0x5678L, 1))

    expected.foreach { case (value, port) => eventQs(port).enqueue(value) }

    waitUntil(eventQs.forall(_.isEmpty))
    waitUntil(dut.writeSlot.toInt >= 1)
    sleepCycles(80)

    assert(!dut.sampleLost.toBoolean)
    assert(!dut.dmaError.toBoolean)

    val frames = readFrames(memory, 4)
    expected.foreach { case (value, port) =>
      assert(frames.exists {
        case DecodedSample(event, src, _) => event == value && src == port
        case _: DecodedMarker => false
      },
        s"missing flushed trace sample value=$value port=$port in ${frames.mkString("; ")}")
    }
    val bubbles = frames.collect { case marker: DecodedMarker if marker.isBubble => marker }
    assert(bubbles.nonEmpty, s"expected bubble samples in flushed beat: ${frames.mkString("; ")}")
  }

  test("wraps and overwrites the start of the DRAM buffer") { implicit dut =>
    val (eventQs, memory) = setup()
    val framesPerBeat = axiDataWidth / sampleWidth
    val wrapBeats = 2
    val total = (bufferSlots + wrapBeats) * framesPerBeat

    0 until total foreach { idx =>
      eventQs(0).enqueue(idx + 1)
      sleepCycles(16)
    }

    waitUntil(eventQs.forall(_.isEmpty))

    val expectedAtBase = ((total - wrapBeats * framesPerBeat + 1) to total).map(_.toLong)
    var framesAtBase = readFrames(memory, expectedAtBase.length)
    var timeout = 2000
    while (framesAtBase.collect { case DecodedSample(event, _, _) => event } != expectedAtBase && timeout > 0) {
      sleepCycles(1)
      timeout -= 1
      framesAtBase = readFrames(memory, expectedAtBase.length)
    }

    assert(timeout > 0, s"buffer base was not overwritten after wrap: ${framesAtBase.mkString("; ")}")
    assert(dut.writeSlot.toInt == wrapBeats,
      s"write slot should point $wrapBeats beats past the base after wrap, got ${dut.writeSlot.toInt}")
    assert(!dut.sampleLost.toBoolean)
    assert(!dut.dmaError.toBoolean)

    framesAtBase.zip(expectedAtBase).foreach { case (frame, expected) =>
      frame match {
        case DecodedSample(event, src, _) =>
          assert(src == 0, s"wrapped frame came from wrong source: $frame")
          assert(event == expected, s"unexpected wrapped frame: got $frame expected event=$expected")
        case marker: DecodedMarker =>
          fail(s"unexpected marker frame after wrap: $marker")
      }
    }
  }
}
