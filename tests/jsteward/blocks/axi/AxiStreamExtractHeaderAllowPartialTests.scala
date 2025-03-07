package jsteward.blocks.axi

class AxiStreamExtractHeaderAllowPartialTests extends AxiStreamExtractHeaderTestsCommonSetup {
  val headerBytes = 14
  def dutGen = AxiStreamExtractHeader(axisConfig, headerBytes)(headerBytes - 4)

  test("partial headers") { dut =>
    testAbnormalPackets(dut, minHeaderLen = headerBytes - 4, headerBytes)
  }
}
