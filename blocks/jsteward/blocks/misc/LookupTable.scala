package jsteward.blocks.misc

import jsteward.blocks.misc.LookupTable.LookupFunc
import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._

import scala.language.postfixOps

object LookupTable {
  /** (stored value, query) => match? */
  type LookupFunc[VT <: Data, QT <: Data] = (VT, QT) => Bool
}

case class LookupTable[
  VT <: Data,
  QT <: Data,
  UT <: Data,
](valueType: HardType[VT],
  queryType: HardType[QT],
  userDataType: HardType[UT],
  numElems: Int,
  valueInit: VT => (),
  matchFunc: LookupFunc[VT, QT],
 ) extends Component {
  val idxWidth = log2Up(numElems)

  val io = new Bundle {
    val update = slave(Flow(new Bundle {
      val value = valueType()
      val idx = UInt(idxWidth bits)
    }))
    val readbackIdx = in(UInt(idxWidth bits))
    val readback = out(valueType())

    val lookup = slave(Stream(new Bundle {
      val query = queryType()
      val userData = userDataType()
    }))
    val result = master(Stream(new Bundle {
      val matched = Bool()
      val value = valueType()
      val idx = UInt(idxWidth bits)
      val userData = userDataType()
    }))
  }

  val storage = Vec.fill(numElems)(Reg(valueType()))
  storage.foreach(valueInit)

  when (io.update.fire) {
    storage(io.update.idx) := io.update.value
  }
  io.readback := storage(io.readbackIdx)

  val pip = new StagePipeline
  pip.node(0).arbitrateFrom(io.lookup)

  val captureKey = new pip.Area(0) {
    val QUERY = insert(io.lookup.query)
    val USER_DATA = insert(io.lookup.userData)
  }
  import captureKey._

  val parallelMatch = new pip.Area(1) {
    val matchVec = Bits(numElems bits)
    storage zip matchVec.asBools foreach { case (s, m) =>
      m := matchFunc(s, QUERY)
    }

    val MATCH_VEC = insert(matchVec)
  }
  import parallelMatch._

  val calcIdx = new pip.Area(2) {
    val MATCHED = insert(MATCH_VEC.orR)
    val IDX = insert(OHToUInt(MATCH_VEC))
    val VALUE = insert(PriorityMux(MATCH_VEC, storage))
  }
  import calcIdx._

  pip.node(3).driveTo(io.result) { case (op, n) =>
    op.matched := n(MATCHED)
    op.value := n(VALUE)
    op.idx := n(IDX)
    op.userData := n(USER_DATA)
  }

  pip.build()

  /** Latency of a lookup operation.  Can be used e.g. to delay the payload flow before deciding if
    * the payload should be dropped.
    */
  val lookupLatency = pip.nodes.size - 1
}
