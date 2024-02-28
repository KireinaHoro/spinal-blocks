package jsteward.blocks.eci

import spinal.core._

import scala.language.postfixOps

/** Replicates eci_cmd_defs.sv in ECI Toolkit */
object EciCmdDefs {
  val ECI_WORD_WIDTH = 64
  val ECI_CL_WIDTH = 1024
  val ECI_CL_SIZE_BYTES = ECI_CL_WIDTH / 8
  val ECI_CL_LEN_WIDTH = log2Up(ECI_CL_SIZE_BYTES) + 1
  val ECI_PACKET_SIZE = (ECI_CL_WIDTH / ECI_WORD_WIDTH) + 1
  val ECI_PACKET_SIZE_WIDTH = log2Up(ECI_PACKET_SIZE)

  val ECI_TOT_NUM_VCS = 14
  val ECI_LCL_TOT_NUM_VCS_WIDTH = log2Up(ECI_TOT_NUM_VCS) + 1

  val ECI_ADDR_WIDTH = 40
  val ECI_OPCODE_WIDTH = 5
  val ECI_ID_WIDTH = 5
  val ECI_SZ_WIDTH = 3
  val ECI_STSZ_WIDTH = 4
  val ECI_DMASK_WIDTH = 4
  val ECI_BYTE_STRB_WIDTH = 2
  val ECI_HREQID_WIDTH = 6
  val ECI_PACKCNT_WIDTH = 3
  val ECI_PPVID_WIDTH = 6
  val ECI_RTAD_WIDTH = 3
  val ECI_NODE_ID_WIDTH = 2

  def EciClLen = Bits(ECI_CL_LEN_WIDTH bits)
  def EciOpcode = Bits(ECI_OPCODE_WIDTH bits)
  def EciAddress = Bits(ECI_ADDR_WIDTH bits)
  def EciId = Bits(ECI_ID_WIDTH bits)
  def EciSz = Bits(ECI_SZ_WIDTH bits)
  def EciDmask = Bits(ECI_DMASK_WIDTH bits)
  def EciHreqId = Bits(ECI_HREQID_WIDTH bits)
  def EciPackCnt = Bits(ECI_PACKCNT_WIDTH bits)
  def EciStSz = Bits(ECI_STSZ_WIDTH bits)
  def EciByteStrb = Bits(ECI_BYTE_STRB_WIDTH bits)
  def EciPpvid = Bits(ECI_PPVID_WIDTH bits)
  def EciRtad = Bits(ECI_RTAD_WIDTH bits)
  def EciVcSize = Bits(ECI_LCL_TOT_NUM_VCS_WIDTH bits)
  def EciPacketSize = Bits(ECI_PACKET_SIZE_WIDTH bits)
  def EciNodeId = Bits(ECI_NODE_ID_WIDTH bits)
}

import jsteward.blocks.eci.EciCmdDefs._

case class EciVcCatMreq0to10() extends Bundle {
  val opcode = EciOpcode
  val xb4 = Bits(4 bits)
  val rreqId = EciId
  val dmask = EciDmask
  val ns = Bool()
  val xb3 = Bits(3 bits)
  val xb2 = Bits(2 bits)
  val address = EciAddress
}

case class EciVcCatMreq24() extends Bundle {
  val opcode = EciOpcode
  val xb3 = Bits(4 bits)
  val rreqId = EciId
  val xb40 = Bits(40 bits)
  val rtad = EciRtad
  val xb1 = Bool()
  val ppvid = EciPpvid
}

case class EciVcCatMrsp0to2() extends Bundle {
  val opcode = EciOpcode
  val xb10 = Bits(9 bits)
  val dmask = EciDmask
  val ns = Bool()
  val xb5 = Bits(5 bits)
  val address = EciAddress
}

case class EciVcCatMrsp3to8() extends Bundle {
  val opcode = EciOpcode
  val xb3 = Bits(3 bits)
  val hreqId = EciHreqId
  val dmask = EciDmask
  val ns = Bool()
  val xb5 = Bits(5 bits)
  val address = EciAddress
}

case class EciVcCatMrsp24() extends Bundle {
  val opcode = EciOpcode
  val xb12 = Bits(13 bits)
  val ns = Bool()
  val xb35 = Bits(35 bits)
  val rtad = EciRtad
  val xb1 = Bool()
  val ppvid = EciPpvid
}

case class EciVcCatMrsp9to10() extends Bundle {
  val opcode = EciOpcode
  val nxm = Bool()
  val xb3 = Bits(3 bits)
  val rreqId = EciId
  val dmask = EciDmask
  val xb1 = Bool()
  val dirty = Bits(4 bits)
  val xb1_2 = Bool()
  val cacheLineIndex = Bits(33 bits)
  val fillo = Bits(2 bits)
  val xb5 = Bits(5 bits)
}

case class EciVcCatMfwd0to15() extends Bundle {
  val opcode = EciOpcode
  val xb3 = Bits(3 bits)
  val hreqId = EciHreqId
  val dmask = EciDmask
  val ns = Bool()
  val xb1 = Bool()
  val rnode = EciNodeId
  val xb2 = Bits(2 bits)
  val address = EciAddress
}

case class LclMfwd() extends Bundle {
  val opcode = EciOpcode
  val xb3 = Bits(3 bits)
  val hreqId = EciHreqId
  val dmask = EciDmask
  val ns = Bool()
  val xb1 = Bool()
  val rnode = EciNodeId
  val xb2 = Bits(2 bits)
  val address = EciAddress
}

case class LclMrsp0to1() extends Bundle {
  val opcode = EciOpcode
  val xb3 = Bits(3 bits)
  val hreqId = EciHreqId
  val dmask = EciDmask
  val ns = Bool()
  val xb5 = Bits(5 bits)
  val address = EciAddress
}

case class LclMrsp2() extends Bundle {
  val opcode = EciOpcode
  val xb19 = Bits(19 bits)
  val address = EciAddress
}

case class EciWord() extends Union {
  // val mreq0to10 = newElement(EciVcCatMreq0to10())
  val rldi = newElement(EciVcCatMreq0to10())
  val rstt = newElement(EciVcCatMreq0to10())
  val rldx = newElement(EciVcCatMreq0to10())
  val rc2d_o = newElement(EciVcCatMreq0to10())
  val rc2d_s = newElement(EciVcCatMreq0to10())
  val gsync = newElement(EciVcCatMreq24())

  // val mrsp0to2 = newElement(EciVcCatMrsp0to2())
  val vicd = newElement(EciVcCatMrsp0to2())
  val vicc = newElement(EciVcCatMrsp0to2())
  val vics = newElement(EciVcCatMrsp0to2())
  // val mrsp3to8 = newElement(EciVcCatMrsp3to8())
  val vicdhi = newElement(EciVcCatMrsp3to8())
  val hakd = newElement(EciVcCatMrsp3to8())
  val hakn_s = newElement(EciVcCatMrsp3to8())
  val haki = newElement(EciVcCatMrsp3to8())
  val haks = newElement(EciVcCatMrsp3to8())
  val hakn = newElement(EciVcCatMrsp3to8())
  val gsdn = newElement(EciVcCatMrsp24())

  val pemd = newElement(EciVcCatMrsp9to10())
  val psha_new = newElement(EciVcCatMrsp9to10())

  // val mfwdGeneric = newElement(EciVcCatMfwd0to15())
  val fevx_eh = newElement(EciVcCatMfwd0to15())
  val sinv_h = newElement(EciVcCatMfwd0to15())

  // val lclMfwdGeneric = newElement(LclMfwd())
  val lc = newElement(LclMfwd())
  val lci = newElement(LclMfwd())

  // val lclMrsp0to1 = newElement(LclMrsp0to1())
  val lca = newElement(LclMrsp0to1())
  val lcia = newElement(LclMrsp0to1())

  // val lclMrsp2 = newElement(LclMrsp2())
  val ul = newElement(LclMrsp2())

  override def postInitCallback(): EciWord.this.type = {
    super.postInitCallback()
    assert(getBitsWidth == ECI_WORD_WIDTH, s"Actual width of EciWord $getBitsWidth != $ECI_WORD_WIDTH")
    this
  }
}

