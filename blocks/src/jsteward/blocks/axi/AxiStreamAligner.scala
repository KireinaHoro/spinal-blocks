package jsteward.blocks.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis._
import spinal.lib.fsm._
import spinal.lib.misc.pipeline._

/**
 * For a stream that has a gap at the beginning, after process by [[AxiStreamExtractHeader]], shift the stream such
 * that the gap is closed and the stream is aligned.
 * Does not handle "holes" (TKEEP[x] == 0) in the middle of the stream.  Allows the beginning of stream to contain
 * fully-null (TKEEP == 0) beats, but not in the middle.
 *
 * @param axisConfig AXI-Stream config for input and output
 */
case class AxiStreamAligner(axisConfig: Axi4StreamConfig) extends Component {
  val io = new Bundle {
    val input = slave(Axi4Stream(axisConfig))
    val output = master(Axi4Stream(axisConfig))
  }

  assert(!axisConfig.useStrb, "TSTRB not supported")
  assert(!axisConfig.useUser, "TUSER not supported")
  assert(!axisConfig.useId, "TID not supported")
  assert(axisConfig.useLast, "must enable TLAST or we don't know packet boundary")
  assert(axisConfig.useKeep, "must enable TKEEP or there are no gaps to close")

  io.output.assertPersistence()

  // example operation for 64b, shift = 5 (* 8):
  // MSB ............... LSB
  //  1  1  1  0  0  0  0  0
  // AA AA AA xx xx xx xx xx
  // CC CC CC BB BB BB BB BB

  // we need CtrlApi to drop an empty tail
  val pip = new StageCtrlPipeline
  pip.ctrl(0).up.arbitrateFrom(io.input)

  val captureInput = new pip.Ctrl(0) {
    val DATA = insert(io.input.data)
    val KEEP = insert(io.input.keep)
    val LAST = insert(io.input.last)

    // drop NULL beats
    terminateWhen(KEEP === 0)

    val isFirst = Reg(Bool()) init True
    when (down.isFiring) {
      isFirst := LAST
    }
    val FIRST = insert(isFirst)
    val HEAD = insert(FIRST && !LAST)
    val SINGLE = insert(FIRST && LAST)
    val LAST_OF_MANY = insert(LAST && !FIRST)
  }
  import captureInput._

  val calculateShifts = new pip.Ctrl(1) {
    val toShiftFirstBeat = CountTrailingZeroes(KEEP).resize(log2Up(axisConfig.dataWidth))
    val toShift = CombInit(toShiftFirstBeat)
    val toShiftSaved = Reg(toShift) init 0

    when (up.isFiring) {
      when (FIRST) {
        toShiftSaved := toShift
      } otherwise {
        toShift := toShiftSaved
      }
    }

    val TO_SHIFT_KEEP = insert(toShift)
    val TO_SHIFT_DATA = insert((toShift * 8).resize(log2Up(axisConfig.dataWidth * 8)))
  }
  import calculateShifts._

  val calculateHeadTail = new pip.Ctrl(2) {
    // MSB ............... LSB
    // xx xx xx xx xx AA AA AA
    // BB BB BB BB BB CC CC CC
    val shiftedKeep = KEEP.rotateRight(TO_SHIFT_KEEP)
    val shiftedData = DATA.rotateRight(TO_SHIFT_DATA)

    val HEAD_DATA_MASK = insert(io.input.data.getAllTrue |>> TO_SHIFT_DATA)
    val HEAD_KEEP_MASK = insert(io.input.keep.getAllTrue |>> TO_SHIFT_KEEP)
    val HEAD_DATA = insert(shiftedData & HEAD_DATA_MASK)
    val HEAD_KEEP = insert(shiftedKeep & HEAD_KEEP_MASK)

    val tailDataMask = io.input.data.getAllTrue |<< (U(axisConfig.dataWidth * 8) - TO_SHIFT_DATA)
    val tailKeepMask = io.input.keep.getAllTrue |<< (U(axisConfig.dataWidth) - TO_SHIFT_KEEP)
    val TAIL_DATA = insert(shiftedData & tailDataMask)
    val TAIL_KEEP = insert(shiftedKeep & tailKeepMask)
  }
  import calculateHeadTail._

  val assembleBeat = new pip.Ctrl(3) {
    // holds staged HEAD_DATA/HEAD_KEEP from last beat
    val stagingData = Reg(io.output.data) init 0
    val stagingKeep = Reg(io.output.keep) init 0

    val fullData = CombInit(stagingData & HEAD_DATA_MASK | TAIL_DATA)
    val fullKeep = CombInit(stagingKeep & HEAD_KEEP_MASK | TAIL_KEEP)

    when (up.isFiring) {
      stagingData := HEAD_DATA
      stagingKeep := HEAD_KEEP
    }

    when (up.isValid) {
      when (HEAD) {
        // kill first beat when not also LAST
        terminateIt()
      }
    }

    // handle LAST but header keep not 0: need one more beat
    val duplicatedLast = Reg(Bool()) init False
    duplicatedLast.clearWhen(duplicatedLast && down.isFiring)

    val fullLast = CombInit[Bool](LAST)
    when (up.isValid && LAST_OF_MANY && HEAD_KEEP =/= 0 && !duplicatedLast) {
      // this beat is not LAST yet
      fullLast := False
      duplicateIt()
      when (down.isFiring) {
        duplicatedLast := True
      }
    }

    when (fullLast && (duplicatedLast || SINGLE)) {
      fullData := HEAD_DATA
      fullKeep := HEAD_KEEP
    }

    val FULL_DATA = insert(fullData)
    val FULL_KEEP = insert(fullKeep)
    val FULL_LAST = insert(fullLast)

    when (down.isValid) {
      assert(FULL_LAST || FULL_KEEP.andR, "KEEP not fully set in the middle of stream")
    }
  }
  import assembleBeat._

  val po = pip.ctrls.values.last.down
  io.output.data := po(FULL_DATA)
  io.output.keep := po(FULL_KEEP)
  io.output.last := po(FULL_LAST)
  po.arbitrateTo(io.output)

  pip.build()
}
