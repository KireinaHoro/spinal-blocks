package jsteward.blocks.eci

import spinal.core._

object EciDcsDefs {
  // Refer to Chapter 8.3.2 of CCKit
  val DS_ADDR_WIDTH = 38
  val MAX_DCU_ID_WIDTH = 7
  val DS_NUM_SETS_PER_DCU = 128
  val DS_SET_WN_DCU_WIDTH = log2Up(DS_NUM_SETS_PER_DCU)
  val DS_SET_IDX_WIDTH = 13
  val DS_DCU_ID_WIDTH = DS_SET_IDX_WIDTH - DS_SET_WN_DCU_WIDTH
  val DS_NUM_DCU_PER_SLICE = 1 << (DS_DCU_ID_WIDTH - 1)
}
