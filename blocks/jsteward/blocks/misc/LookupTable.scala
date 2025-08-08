package jsteward.blocks.misc

import jsteward.blocks.misc.LookupTable.LookupFunc
import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.bus.regif.AccessType
import spinal.lib.misc.pipeline._

import scala.language.postfixOps

object LookupTable {
  /** (stored key, stored value, lookup key) => match? */
  type LookupFunc[KT <: Data, VT <: Data] = (KT, VT, KT) => Bool
}

case class LookupTable[
  KT <: Data,
  VT <: Data,
  UT <: Data,
](keyType: HardType[KT],
  valueType: HardType[VT],
  userDataType: HardType[UT],
  numElems: Int,
  valueInit: () => VT,
  matchFunc: LookupFunc[KT, VT],
 ) extends Component {
  val idxWidth = log2Up(numElems)

  case class StorageUnit() extends Bundle {
    val key = keyType()
    val value = valueType()
  }

  val io = new Bundle {
    val update = slave(Flow(new Bundle {
      val key = keyType()
      val value = valueType()
      val idx = UInt(idxWidth bits)
    }))
    val readbackIdx = in(UInt(idxWidth bits))
    val readback = out(StorageUnit())

    val lookup = slave(Stream(new Bundle {
      val key = keyType()
      val userData = userDataType()
    }))
    val result = master(Stream(new Bundle {
      val matched = Bool()
      val value = valueType()
      val idx = UInt(idxWidth bits)
      val userData = userDataType()
    }))
  }

  val storage = Vec.fill(numElems)(Reg(StorageUnit()))
  storage.foreach { _.value init valueInit() }

  when (io.update.fire) {
    storage(io.update.idx).key := io.update.key
    storage(io.update.idx).value := io.update.value
  }
  io.readback.assignAllByName(storage(io.readbackIdx))

  val pip = new StagePipeline
  pip.node(0).arbitrateFrom(io.lookup)

  val captureKey = new pip.Area(0) {
    val KEY = insert(io.lookup.key)
    val USER_DATA = insert(io.lookup.userData)
  }
  import captureKey._

  val parallelMatch = new pip.Area(1) {
    val matchVec = Bits(numElems bits)
    storage zip matchVec.asBools foreach { case (s, m) =>
      m := matchFunc(s.key, s.value, KEY)
    }

    val MATCH_VEC = insert(matchVec)
  }
  import parallelMatch._

  val calcIdx = new pip.Area(2) {
    val MATCHED = insert(MATCH_VEC.orR)
    val IDX = insert(OHToUInt(MATCH_VEC))
    val VALUE = insert(PriorityMux(MATCH_VEC, storage.map(_.value)))
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
