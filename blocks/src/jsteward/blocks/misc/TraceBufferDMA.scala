package jsteward.blocks.misc

import jsteward.blocks.axi._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axis._
import spinal.lib.fsm._

import scala.language.postfixOps

/** Record transactions on multiple `Flow[T]` and stream them to DRAM through
  * an AXI DMA writer.
  *
  * Each trace sample contains the opaque payload, a source id, and a timestamp.
  * Samples are packed as a bitstream into full AXI beats before they are passed
  * to the DMA writer.
  */
case class TraceBufferDMA[T <: Data](
                                      payloadType: HardType[T],
                                      numInputs: Int,
                                      axiConfig: Axi4Config,
                                      axiBufferBase: BigInt,
                                      axiBufferSize: BigInt,
                                      burstFifoSize: Int = 16,
                                      frameFifoSize: Int = 32,
                                      timestampWidth: Int = 64,
                                      lostCountWidth: Int = 32,
                                      axiMaxBurstLen: Int = 16,
                                    ) extends Component {
  require(numInputs > 0, "TraceBufferDMA needs at least one trace input")
  require(axiMaxBurstLen > 1, "TraceBufferDMA needs at least two beats per DMA burst")
  require(axiConfig.dataWidth % 8 == 0, "AXI data width must be byte-aligned")
  require((axiBufferBase & (axiConfig.dataWidth / 8 - 1)) == 0, "AXI buffer base must be AXI-word aligned")
  require((axiBufferSize & (axiConfig.dataWidth / 8 - 1)) == 0, "AXI buffer size must be AXI-word aligned")
  require(axiBufferSize >= 2 * (axiConfig.dataWidth / 8), "TraceBufferDMA needs at least two AXI words of buffer space")
  require(axiBufferBase + axiBufferSize <= (BigInt(1) << axiConfig.addressWidth), "AXI buffer exceeds address space")
  assert(!axiConfig.useQos, "axi dma does not support qos")
  assert(!axiConfig.useRegion, "axi dma does not support region")

  val busBytes = axiConfig.dataWidth / 8
  val busByteShift = log2Up(busBytes)
  val payloadWidth = payloadType().getBitsWidth
  // Reserve the all-ones source id for the lost-samples count frame.
  val sourceWidth = log2Up(numInputs + 1)
  val lostSourceId = (BigInt(1) << sourceWidth) - 1
  val sampleWidth = payloadWidth + sourceWidth + timestampWidth
  require(numInputs <= lostSourceId, "the all-ones source id is reserved for lost-samples count frames")
  require(lostCountWidth <= payloadWidth, "lost count must fit in the trace payload field")
  require(sampleWidth <= axiMaxBurstLen * axiConfig.dataWidth, "trace sample is wider than the maximum AXI burst")
  val bufferSlotsBigInt = axiBufferSize / busBytes
  require(bufferSlotsBigInt <= Int.MaxValue, "AXI buffer size is too large")
  val bufferSlots = bufferSlotsBigInt.toInt

  case class CapturedFrame() extends Bundle {
    val event = payloadType()
    val src = UInt(sourceWidth bits)
    val ts = UInt(timestampWidth bits)
    val tracesLost = Bool()
    val lostCount = UInt(lostCountWidth bits)
  }

  val traceIn = Vec(slave(Flow(payloadType)), numInputs)
  val axi = master(Axi4(axiConfig))
  val sampleLost = out(RegInit(False))
  val dmaError = out(RegInit(False))
  val writeSlot = out(UInt(log2Up(bufferSlots) bits))

  val maxBeatsPerDesc = scala.math.min(axiMaxBurstLen, bufferSlots)
  val maxDescBytes = maxBeatsPerDesc * busBytes
  val lenWidth = log2Up(maxDescBytes + 1)
  val tagWidth = 1

  val axisConfig = Axi4StreamConfig(
    dataWidth = busBytes,
    useKeep = true,
    useLast = true,
  )
  val dmaConfig = AxiDmaConfig(
    axiConfig = axiConfig,
    axisConfig = axisConfig,
    axiMaxBurstLen = axiMaxBurstLen,
    lenWidth = lenWidth,
    tagWidth = tagWidth,
  )

  val cycleCount = CounterFreeRun(timestampWidth bits)

  val catPorts = Flow(traceIn)
  catPorts.valid := traceIn.map(_.valid).orR
  catPorts.payload zip traceIn foreach { case (cp, p) => cp := p }

  val overflow = Bool()
  val bufferedPorts = catPorts.toStream(overflow, burstFifoSize, burstFifoSize)
  bufferedPorts.ready := False
  sampleLost.setWhen(overflow)

  val pendingLostCount = Reg(UInt(lostCountWidth bits)) init 0
  val clearLostCount = Bool()
  clearLostCount := False
  val lostBase = Mux(clearLostCount, U(0, lostCountWidth bits), pendingLostCount)
  val lostIncrement = CountOne(traceIn.map(_.valid)).resize(lostCountWidth + 1)
  val lostSum = lostBase.resize(lostCountWidth + 1) + lostIncrement
  when(clearLostCount || overflow) {
    when(overflow) {
      pendingLostCount := Mux(lostSum.msb, U((BigInt(1) << lostCountWidth) - 1, lostCountWidth bits), lostSum.resized)
    } otherwise {
      pendingLostCount := 0
    }
  }

  val savedPorts = bufferedPorts.toFlowFire.toReg()
  savedPorts.foreach { sp => sp.valid init False }
  val savedTs = RegNextWhen(cycleCount.value, bufferedPorts.fire)
  val nextPort = OHToUInt(OHMasking.first(savedPorts.map(_.valid)))

  val frameSource = Stream(CapturedFrame())
  frameSource.valid := False
  frameSource.payload.clearAll()

  val captureFsm = new StateMachine {
    val idle: State = new State with EntryPoint {
      whenIsActive {
        when(pendingLostCount =/= 0) {
          goto(emitLost)
        } otherwise {
          bufferedPorts.ready := True
          when(bufferedPorts.valid) {
            goto(emitSaved)
          }
        }
      }
    }

    val emitLost: State = new State {
      whenIsActive {
        frameSource.valid := True
        frameSource.tracesLost := True
        frameSource.lostCount := pendingLostCount
        frameSource.src := U(lostSourceId, sourceWidth bits)
        frameSource.ts := cycleCount.value
        when(frameSource.ready) {
          clearLostCount := True
          goto(idle)
        }
      }
    }

    val emitSaved: State = new State {
      whenIsActive {
        frameSource.valid := True
        frameSource.event := savedPorts(nextPort).payload
        frameSource.src := nextPort.resized
        frameSource.ts := savedTs
        when(frameSource.ready) {
          savedPorts(nextPort).valid := False
          when(CountOne(savedPorts.map(_.valid)) === 1) {
            goto(idle)
          }
        }
      }
    }
  }

  val framesToDma = frameSource.queue(frameFifoSize)

  def packSample(frame: CapturedFrame): Bits = {
    val ret = Bits(sampleWidth bits)
    val normalPayload = Bits(payloadWidth bits)
    val lostPayload = Bits(payloadWidth bits)
    ret.clearAll()
    normalPayload := frame.event.asBits.resized
    lostPayload := frame.lostCount.asBits.resized

    ret(payloadWidth - 1 downto 0) := Mux(frame.tracesLost, lostPayload, normalPayload)
    ret(payloadWidth + sourceWidth - 1 downto payloadWidth) := frame.src.asBits
    ret(sampleWidth - 1 downto payloadWidth + sourceWidth) := frame.ts.asBits
    ret
  }

  val packedBeats = Stream(Bits(axiConfig.dataWidth bits))
  val packerWidth = axiConfig.dataWidth + sampleWidth
  val packerData = Reg(Bits(packerWidth bits)) init 0
  val packerBits = Reg(UInt(log2Up(packerWidth + 1) bits)) init 0
  val packedSample = packSample(framesToDma.payload)
  val beatWillFire = packedBeats.valid && packedBeats.ready
  val bitsAfterBeat = packerBits - Mux(beatWillFire, U(axiConfig.dataWidth, packerBits.getWidth bits), U(0, packerBits.getWidth bits))
  val roomAfterBeat = U(packerWidth, packerBits.getWidth + 1 bits) - bitsAfterBeat.resize(packerBits.getWidth + 1)

  packedBeats.valid := packerBits >= axiConfig.dataWidth
  packedBeats.payload := packerData(axiConfig.dataWidth - 1 downto 0)
  packedBeats.ready := False
  framesToDma.ready := roomAfterBeat >= sampleWidth

  when(beatWillFire || framesToDma.fire) {
    val dataAfterBeat = Mux(beatWillFire, (packerData.asUInt >> axiConfig.dataWidth).resize(packerWidth).asBits, packerData)
    val appendedSample = (packedSample.asUInt.resize(packerWidth) |<< bitsAfterBeat).asBits
    packerData := Mux(framesToDma.fire, dataAfterBeat | appendedSample, dataAfterBeat)
    packerBits := bitsAfterBeat + Mux(framesToDma.fire, U(sampleWidth, packerBits.getWidth bits), U(0, packerBits.getWidth bits))
  }

  val axiDma = new AxiDma(dmaConfig)
  axiDma.io.m_axi >> axi
  axiDma.io.read_enable := False
  axiDma.io.write_enable := True
  axiDma.io.write_abort := False
  axiDma.s_axis_read_desc.valid := False
  axiDma.s_axis_read_desc.payload.clearAll()
  axiDma.m_axis_read_data.ready := False

  dmaError.setWhen(axiDma.m_axis_write_desc_status.valid && axiDma.m_axis_write_desc_status.error =/= 0)

  axiDma.s_axis_write_desc.valid := False
  axiDma.s_axis_write_desc.payload.clearAll()
  axiDma.s_axis_write_data.valid := False
  axiDma.s_axis_write_data.payload.clearAll()

  val slot = Reg(UInt(log2Up(bufferSlots) bits)) init 0
  writeSlot := slot

  val beatsInDesc = Reg(UInt(log2Up(maxBeatsPerDesc + 1) bits)) init maxBeatsPerDesc
  val beatInDesc = Reg(UInt(log2Up(maxBeatsPerDesc) bits)) init 0

  val slotsUntilWrap = U(bufferSlots, log2Up(bufferSlots + 1) bits) - slot.resize(log2Up(bufferSlots + 1))
  val nextDescBeats = UInt(log2Up(maxBeatsPerDesc + 1) bits)
  nextDescBeats := slotsUntilWrap.resized
  when(slotsUntilWrap > maxBeatsPerDesc) {
    nextDescBeats := maxBeatsPerDesc
  }

  val currentAddress = (U(axiBufferBase, axiConfig.addressWidth bits) +
    (slot.resize(axiConfig.addressWidth) << busByteShift)).resized

  val dmaFsm = new StateMachine {
    val issueDesc: State = new State with EntryPoint {
      whenIsActive {
        axiDma.s_axis_write_desc.valid := True
        axiDma.s_axis_write_desc.addr := currentAddress
        axiDma.s_axis_write_desc.len := (nextDescBeats.resize(lenWidth) << busByteShift).resized
        axiDma.s_axis_write_desc.tag.clearAll()
        when(axiDma.s_axis_write_desc.ready) {
          beatsInDesc := nextDescBeats
          beatInDesc := 0
          goto(writeFrames)
        }
      }
    }

    val writeFrames: State = new State {
      whenIsActive {
        axiDma.s_axis_write_data.valid := packedBeats.valid
        axiDma.s_axis_write_data.payload.data := packedBeats.payload
        axiDma.s_axis_write_data.payload.keep.setAll()
        axiDma.s_axis_write_data.payload.last := beatInDesc === (beatsInDesc - 1).resized
        packedBeats.ready := axiDma.s_axis_write_data.ready

        when(axiDma.s_axis_write_data.fire) {
          when(beatInDesc === (beatsInDesc - 1).resized) {
            val nextSlot = slot.resize(log2Up(bufferSlots + 1)) + beatsInDesc.resize(log2Up(bufferSlots + 1))
            when(nextSlot === bufferSlots) {
              slot := 0
            } otherwise {
              slot := nextSlot.resized
            }
            goto(issueDesc)
          } otherwise {
            beatInDesc := beatInDesc + 1
          }
        }
      }
    }
  }
}
