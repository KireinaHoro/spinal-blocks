package jsteward.blocks.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis._
import spinal.lib.misc.pipeline._
import spinal.lib.fsm._

import scala.language.postfixOps

/**
 * Extract (up to) the first [[maxHeaderLen]] bytes from an [[Axi4Stream]]; these bytes will be stripped from the output
 * stream.  Assumes that any NULL bytes are only present at the beginning of the stream.  This module is intended to be chained.
 * To enable partial header output, set [[minHeaderLen]] smaller than [[maxHeaderLen]].  This feature can be used to
 * handle IP headers, for example.
 *
 * The module guarantees that no output beats will happen before the header is sent.
 *
 * @param axisConfig   AXI-Stream config for input and output
 * @param maxHeaderLen maximum length of header, in bytes
 * @param minHeaderLen minimum length of header, in bytes; default to [[maxHeaderLen]] (no partial headers)
 */
case class AxiStreamExtractHeader(axisConfig: Axi4StreamConfig, maxHeaderLen: Int)(minHeaderLen: Int = maxHeaderLen) extends Component {
  assert(minHeaderLen <= maxHeaderLen, s"minimum header length $minHeaderLen bigger than maximum header length $maxHeaderLen")

  val io = new Bundle {
    val input = slave(Axi4Stream(axisConfig))
    // FIXME: should put header inside TUSER of output instead of as a separate stream;
    //        timing requirements between header and output is fragile and will break if
    //        header gets pipelined
    val output = master(Axi4Stream(axisConfig))
    val outputAck = in(Bool())
    val header = master(Stream(Bits(maxHeaderLen * 8 bits)))
    val statistics = out(new Bundle {
      val headerOnly = Reg(UInt(64 bits))
      val partialHeader = Reg(UInt(64 bits))
      val incompleteHeader = Reg(UInt(64 bits))
      val normalPackets = Reg(UInt(64 bits))
    })
  }

  def inc(f: io.statistics.type => UInt) = {
    f(io.statistics) := f(io.statistics) + 1
  }

  io.statistics.flatten.foreach(_ init U(0))

  assert(!axisConfig.useStrb, "TSTRB not supported")
  assert(axisConfig.useKeep, "must enable TKEEP or we can't strip part of beat")
  assert(axisConfig.useLast, "must enable TLAST or we don't know packet boundary")
  assert(axisConfig.useKeep || maxHeaderLen % axisConfig.dataWidth == 0, "either enable TKEEP, or the header should be a multiple of TDATA")

  io.output.assertPersistence()
  io.header.assertPersistence()

  // we shouldn't output completely NULL beats
  assert(!io.output.valid || io.output.keep =/= 0, "output should not have NULL beats")
  // we also do not handle completely NULL beats
  assert(!io.input.valid || io.input.keep =/= 0, "input should not have NULL beats")

  io.header.setIdle()

  val headerLenWidth = log2Up(maxHeaderLen) + 1
  val segmentLenWidth = log2Up(axisConfig.dataWidth) + 1

  // pipeline to calculate fragment of header from data and keep
  val pip = new StageCtrlPipeline
  pip.ctrl(0).up.arbitrateFrom(io.input)

  val calculateShifts = new pip.Ctrl(0) {
    val DATA = insert(io.input.data)
    val KEEP = insert(io.input.keep)
    val LAST = insert(io.input.last)

    val hdrLeft = Reg(UInt(headerLenWidth bits)) init maxHeaderLen

    val SEG_OFF = insert(CountTrailingZeroes(KEEP).resize(segmentLenWidth))
    val SEG_LEN = insert(CountOne(KEEP).resize(segmentLenWidth))

    val maskReq = ((U(1) << hdrLeft) - 1).asBits.resize(axisConfig.dataWidth)
    val maskAvail = KEEP >> SEG_OFF
    val KEEP_MASK = insert(maskReq & maskAvail)

    // did we consume the entire beat?
    val BEAT_CONSUMED = insert(hdrLeft >= SEG_LEN)

    val hdrLeftNext = (hdrLeft > SEG_LEN) ? (hdrLeft - SEG_LEN.resized) | U(0)
    when (up.isFiring) {
      when (LAST) {
        hdrLeft := maxHeaderLen
      } otherwise {
        hdrLeft := hdrLeftNext
      }
    }

    // store current header pointer for storeHeader
    val HDR_LEFT = insert(hdrLeft)
    val HDR_LEFT_NEXT = insert(hdrLeftNext)
  }
  import calculateShifts._

  val storeHeader = new pip.Ctrl(1) {
    val hdrBuf = Reg(io.header.payload) init 0

    val MASKED_KEEP = insert(KEEP & ~(KEEP_MASK |<< SEG_OFF))

    val hdrFrag = (DATA >> (SEG_OFF * 8)).resize(maxHeaderLen * 8)
    val hdrFragShifted = hdrFrag |<< ((U(maxHeaderLen) - HDR_LEFT) * 8)
    val HDR_OUT = insert(hdrBuf | hdrFragShifted)

    when (up.isFiring) {
      when (LAST) {
        hdrBuf.clearAll()
      } otherwise {
        hdrBuf := HDR_OUT
      }
    }

    val hdrVariableLen = maxHeaderLen - minHeaderLen
    val HDR_FULL        = insert(HDR_LEFT_NEXT === 0)
    val HDR_PARTIAL     = insert(HDR_LEFT_NEXT > 0 && HDR_LEFT_NEXT <= hdrVariableLen)
    val HDR_INCOMPLETE  = insert(HDR_LEFT_NEXT > hdrVariableLen)
  }
  import storeHeader._

  val outputHeader = new pip.Ctrl(2) {
    val fsm = new StateMachine {
      val idle: State = new State with EntryPoint {
        whenIsActive {
          when (up.isValid) {
            when (HDR_FULL || (HDR_PARTIAL && LAST)) {
              // we have a header to send
              io.header.valid := True
              io.header.payload := HDR_OUT
              when (io.header.ready) {
                goto(passBody)
              } otherwise {
                haltIt()
                goto(waitHeaderAck)
              }
            }
          }
        }
      }
      val waitHeaderAck = new State {
        whenIsActive {
          haltIt()
          io.header.valid := True
          io.header.payload := HDR_OUT
          when (io.header.ready) {
            goto(passBody)
          }
        }
      }
      val passBody = new State {
        whenIsActive {
          when (up.isFiring && LAST) {
            when (HDR_INCOMPLETE) {
              inc(_.incompleteHeader)
            }.elsewhen (HDR_PARTIAL) {
              inc(_.partialHeader)
            }.elsewhen (BEAT_CONSUMED) {
              inc(_.headerOnly)
            }.otherwise {
              inc(_.normalPackets)
            }
          }
          when (io.outputAck) {
            goto(idle)
          }
        }
      }
    }
    fsm.build()

    // if we consumed the entire beat, do not pass through
    terminateWhen(BEAT_CONSUMED)
  }

  // insert one extra stage to make sure output is delayed by one cycle after header;
  // this also interrupts timing paths
  new pip.Ctrl(3)

  val po = pip.ctrls.values.last.down

  // keep with the captured fragment removed
  io.output.keep := po(MASKED_KEEP)
  io.output.data := po(DATA)
  io.output.last := po(LAST)
  po.arbitrateTo(io.output)

  pip.build()

  // allow better floor-planning
  addAttribute("keep_hierarchy", "TRUE")
}
