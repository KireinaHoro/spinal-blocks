package jsteward.blocks.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis._

case class AxiStreamDropFrame(axisConfig: Axi4StreamConfig) extends Component {
  val io = new Bundle {
    val input = slave(Axi4Stream(axisConfig))
    val output = master(Axi4Stream(axisConfig))
    val drop = slave Flow(Bool())
  }

  assert(axisConfig.useLast, "must enable TLAST or we don't know packet boundary")

  val doDrop = Reg(Bool()) init False
  doDrop.setWhen(io.drop.valid && io.drop.payload)
  doDrop.clearWhen(io.input.lastFire || (io.drop.valid && !io.drop.payload))

  io.output << io.input.throwWhen(doDrop)
}
