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
  * Each trace occupies one full AXI data beat.  This wastes the unused high
  * bits when traces are narrower than the AXI bus, but keeps every DMA transfer
  * full-width and lets the underlying DMA issue maximum-length INCR bursts.
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
  val bufferSlotsBigInt = axiBufferSize / busBytes
  require(bufferSlotsBigInt <= Int.MaxValue, "AXI buffer size is too large")
  val bufferSlots = bufferSlotsBigInt.toInt

  case class CapturedFrame() extends Bundle {
    val event = payloadType()
    val src = UInt(log2Up(numInputs) bits)
    val ts = UInt(timestampWidth bits)
    val tracesLost = Bool()
    val lostCount = UInt(lostCountWidth bits)
  }

  val traceIn = Vec(slave(Flow(payloadType)), numInputs)
  val axi = master(Axi4(axiConfig))
  val sampleLost = out(RegInit(False))
  val dmaError = out(RegInit(False))
  val writeSlot = out(UInt(log2Up(bufferSlots) bits))

  val maxFramesPerDesc = scala.math.min(axiMaxBurstLen, bufferSlots)
  val maxDescBytes = maxFramesPerDesc * busBytes
  val lenWidth = log2Up(maxDescBytes + 1)
  val tagWidth = 1
  val frameWidth = payloadType().getBitsWidth + log2Up(numInputs) + timestampWidth + 1 + lostCountWidth
  require(frameWidth <= axiConfig.dataWidth, s"captured trace frame is $frameWidth bits, wider than AXI data width ${axiConfig.dataWidth}")

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
        frameSource.src := nextPort
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
  framesToDma.ready := False

  val slot = Reg(UInt(log2Up(bufferSlots) bits)) init 0
  writeSlot := slot

  val framesInDesc = Reg(UInt(log2Up(maxFramesPerDesc + 1) bits)) init maxFramesPerDesc
  val frameInDesc = Reg(UInt(log2Up(maxFramesPerDesc) bits)) init 0

  val slotsUntilWrap = U(bufferSlots, log2Up(bufferSlots + 1) bits) - slot.resize(log2Up(bufferSlots + 1))
  val nextDescFrames = UInt(log2Up(maxFramesPerDesc + 1) bits)
  nextDescFrames := slotsUntilWrap.resized
  when(slotsUntilWrap > maxFramesPerDesc) {
    nextDescFrames := maxFramesPerDesc
  }

  val currentAddress = (U(axiBufferBase, axiConfig.addressWidth bits) +
    (slot.resize(axiConfig.addressWidth) << busByteShift)).resized

  def packFrame(frame: CapturedFrame): Bits = {
    val ret = Bits(axiConfig.dataWidth bits)
    var offset = 0
    ret.clearAll()
    ret(offset + payloadType().getBitsWidth - 1 downto offset) := frame.event.asBits
    offset += payloadType().getBitsWidth
    ret(offset + log2Up(numInputs) - 1 downto offset) := frame.src.asBits
    offset += log2Up(numInputs)
    ret(offset + timestampWidth - 1 downto offset) := frame.ts.asBits
    offset += timestampWidth
    ret(offset) := frame.tracesLost
    offset += 1
    ret(offset + lostCountWidth - 1 downto offset) := frame.lostCount.asBits
    ret
  }

  val dmaFsm = new StateMachine {
    val issueDesc: State = new State with EntryPoint {
      whenIsActive {
        axiDma.s_axis_write_desc.valid := True
        axiDma.s_axis_write_desc.addr := currentAddress
        axiDma.s_axis_write_desc.len := (nextDescFrames.resize(lenWidth) << busByteShift).resized
        axiDma.s_axis_write_desc.tag.clearAll()
        when(axiDma.s_axis_write_desc.ready) {
          framesInDesc := nextDescFrames
          frameInDesc := 0
          goto(writeFrames)
        }
      }
    }

    val writeFrames: State = new State {
      whenIsActive {
        axiDma.s_axis_write_data.valid := framesToDma.valid
        axiDma.s_axis_write_data.payload.data := packFrame(framesToDma.payload)
        axiDma.s_axis_write_data.payload.keep.setAll()
        axiDma.s_axis_write_data.payload.last := frameInDesc === (framesInDesc - 1).resized
        framesToDma.ready := axiDma.s_axis_write_data.ready

        when(axiDma.s_axis_write_data.fire) {
          when(frameInDesc === (framesInDesc - 1).resized) {
            val nextSlot = slot.resize(log2Up(bufferSlots + 1)) + framesInDesc.resize(log2Up(bufferSlots + 1))
            when(nextSlot === bufferSlots) {
              slot := 0
            } otherwise {
              slot := nextSlot.resized
            }
            goto(issueDesc)
          } otherwise {
            frameInDesc := frameInDesc + 1
          }
        }
      }
    }
  }
}
