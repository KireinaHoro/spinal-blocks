package jsteward.blocks.misc.sim

import jsteward.blocks.misc.StreamDispatcherWithEnable
import jsteward.blocks.tagobjects.Formal
import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.formal._

import scala.language.postfixOps

class FormalStreamDispatcherWithEnable extends SpinalFormalFunSuite {
  def tester(): Unit = {
    FormalConfig
      .withBMC(10)
      .withProve(10)
      .withCover(10)
      .withDebug
      .doVerify(new Component {
        val input = slave Stream(UInt(8 bits))
        val mask = in Bits(4 bits)
        val maskChanged = in Bool()
        val outputs = Vec(master Stream(UInt(8 bits)), 4)
        val dut = FormalDut(new Component {
          val input = slave Stream(UInt(8 bits))
          val mask = in Bits(4 bits)
          val maskChanged = in Bool()
          val outputs = Vec(master Stream(UInt(8 bits)), 4)

          outputs <> StreamDispatcherWithEnable(input, 4, mask, maskChanged)
        })

        dut.input <> input
        dut.outputs <> outputs
        dut.mask := mask
        dut.maskChanged := maskChanged

        assumeInitial(ClockDomain.current.isResetActive)

        // XXX: WIP; need to better understand how to write assumptions/asserts

        // drive mask changed when mask changes
        when (changed(mask)) {
          assume(maskChanged)
        }

        // no packet should arrive when we are updating the mask
        when (maskChanged) {
          assume(!input.valid)
        }

        val maskUpdatedAtLeastOnce = Reg(Bool()) init False
        when (!maskUpdatedAtLeastOnce) {
          maskUpdatedAtLeastOnce := maskChanged.rise()
        }
        assume(maskUpdatedAtLeastOnce)

        val data = anyconst(UInt(4 bits))
        input.formalAssumesSlave()
        outputs.foreach(_.formalAssertsMaster())

        // inactive ports should not have transaction
        RegNext(mask).asBools.zip(outputs).foreach { case (en, o) =>
          when (!en) {
            assert(!o.valid)
          }
        }
      })
  }

  test("basic-test", Formal) {
    tester()
  }
}
