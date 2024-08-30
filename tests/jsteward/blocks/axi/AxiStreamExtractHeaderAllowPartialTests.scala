package jsteward.blocks.axi

class AxiStreamExtractHeaderAllowPartialTests extends AxiStreamExtractHeaderTestsCommonSetup {
  def dutGen = AxiStreamExtractHeader(axisConfig, headerBytes, allowPartial = true)

  test("partial headers") { dut =>
    testAbnormalPackets(dut, expectHeader = true)
  }
}
