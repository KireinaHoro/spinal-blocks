package jsteward.blocks.eci

import spinal.core._
import spinal.lib._

/** Interface to the ECI interrupt controller for IPI interrupts.  Same format
  * for FPGA->CPU and CPU->FPGA.
  */
case class EciIntcInterface() extends Bundle {
  val cmd = Bits(8 bits)
  val intId = Bits(4 bits)
  val affLvl0 = Bits(16 bits)
  val affLvl1 = Bits(4 bits)
}
