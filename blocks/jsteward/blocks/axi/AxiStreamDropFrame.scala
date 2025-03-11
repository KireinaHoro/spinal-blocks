package jsteward.blocks.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis._

// TODO: test cases
case class AxiStreamDropFrame(axisConfig: Axi4StreamConfig) extends Component {
  val io = new Bundle {
    val input = slave(Axi4Stream(axisConfig))
    val output = master(Axi4Stream(axisConfig))
    val drop = slave Flow(Bool())
  }

  assert(axisConfig.useLast, "must enable TLAST or we don't know packet boundary")

  val setDrop = io.drop.valid && io.drop.payload
  val doDrop = Reg(Bool()) init False
  doDrop.setWhen(setDrop)

  // don't clear drop when it's just asked for this cycle
  // prevents letting the second one of two last-only streams escape
  doDrop.clearWhen((io.input.lastFire && !setDrop) || (io.drop.valid && !io.drop.payload))

  io.output << io.input.throwWhen(doDrop)
}
