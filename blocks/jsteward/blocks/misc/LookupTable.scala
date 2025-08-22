package jsteward.blocks.misc

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._

import scala.language.postfixOps

case class LookupTable[VT <: Data](valueType: HardType[VT], numElems: Int)(valueInit: VT => Unit) extends Component {
  val idxWidth = log2Up(numElems)

  val update = slave(Flow(new Bundle {
    val value = valueType()
    val idx = UInt(idxWidth bits)
  }))
  val readbackIdx = in(UInt(idxWidth bits))
  val readback = out(valueType())

  val storage = Vec.fill(numElems)(Reg(valueType()))
  storage.foreach(valueInit)

  when (update.fire) {
    storage(update.idx) := update.value
  }
  readback := storage(readbackIdx)

  def makePort[QT <: Data, UT <: Data](queryType: HardType[QT], userDataType: HardType[UT],
                                       name: String = "",
                                       singleMatch: Boolean = false)
                                      (matchFunc: (VT, QT, Int) => Bool) = this.rework {
    val gen = new Area {
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

      val pip = new StagePipeline
      pip.node(0).arbitrateFrom(lookup)

      val captureKey = new pip.Area(0) {
        val QUERY = insert(lookup.query)
        val USER_DATA = insert(lookup.userData)
      }
      import captureKey._

      val parallelMatch = new pip.Area(1) {
        val matchVec = Bits(numElems bits)
        (storage zip matchVec.asBools).zipWithIndex.foreach { case ((s, m), idx) =>
          m := matchFunc(s, QUERY, idx)
        }
        singleMatch generate {
          when (isValid) {
            assert(CountOne(matchVec) === 1, "single match query should match exactly one element")
          }
        }

        val MATCH_VEC = insert(matchVec)
      }
      import parallelMatch._

      val calcIdx = new pip.Area(2) {
        val MATCHED = insert(MATCH_VEC.orR)
        val IDX = insert(if (singleMatch) {
          OHToUInt(MATCH_VEC)
        } else {
          OHToUInt(OHMasking.firstV2[Bits](MATCH_VEC))
        })
        val VALUE = insert(PriorityMux(MATCH_VEC, storage))
      }
      import calcIdx._

      pip.node(3).driveTo(result) { case (op, n) =>
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
    }.setName(name)

    (gen.lookup, gen.result, gen.lookupLatency)
  }
}
