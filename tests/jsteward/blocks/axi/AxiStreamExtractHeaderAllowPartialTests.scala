package jsteward.blocks.axi

class AxiStreamExtractHeaderAllowPartialTests extends AxiStreamExtractHeaderTestsCommonSetup {
  def dutGen = AxiStreamExtractHeader(axisConfig, headerBytes)(headerBytes - 4)

  test("partial headers") { dut =>
    testAbnormalPackets(dut, minHeaderLen = headerBytes - 4)
  }
}
