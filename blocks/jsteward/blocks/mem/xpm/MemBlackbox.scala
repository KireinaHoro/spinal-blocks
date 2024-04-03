package jsteward.blocks.mem.xpm

import jsteward.blocks.misc.checkParam
import spinal.core._

import scala.language.postfixOps

class XpmMemoryDpDistRam(clockDomainA: ClockDomain,
                         clockDomainB: ClockDomain,
                         val addrWidthA: Int = 6,
                         val addrWidthB: Int = 6,
                         val byteWriteWidthA: Int = 32,
                         val clockingMode: String = "common_clock",
                         val ignoreInitSynth: Boolean = false,
                         val memoryInitFile: String = "none",
                         val memoryInitParam: String = "0",
                         val memoryOptimization: String = "true",
                         val memorySize: Int = 2048,
                         val messageControl: Boolean = false,
                         val readDataWidthA: Int = 32,
                         val readDataWidthB: Int = 32,
                         val readLatencyA: Int = 2,
                         val readLatencyB: Int = 2,
                         val readResetValueA: String = "0",
                         val readResetValueB: String = "0",
                         val simAssertChk: Boolean = false,
                         val useEmbeddedConstraint: Boolean = false,
                         val useMemInit: Boolean = true,
                         val useMemInitMmi: Boolean = false,
                         val writeDataWidthA: Int = 32,
                        ) extends BlackBox {
  checkParam(clockingMode)("common_clock", "independent_clock")
  checkParam(memoryOptimization)("true", "false")

  addGenerics(
    "ADDR_WIDTH_A" -> addrWidthA,
    "ADDR_WIDTH_B" -> addrWidthB,
    "BYTE_WRITE_WIDTH_A" -> byteWriteWidthA,
    "CLOCKING_MODE" -> clockingMode,
    "IGNORE_INIT_SYNTH" -> ignoreInitSynth,
    "MEMORY_INIT_FILE" -> memoryInitFile,
    "MEMORY_INIT_PARAM" -> memoryInitParam,
    "MEMORY_OPTIMIZATION" -> memoryOptimization,
    "MEMORY_SIZE" -> memorySize,
    "MESSAGE_CONTROL" -> messageControl,
    "READ_DATA_WIDTH_A" -> readDataWidthA,
    "READ_DATA_WIDTH_B" -> readDataWidthB,
    "READ_LATENCY_A" -> readLatencyA,
    "READ_LATENCY_B" -> readLatencyB,
    "READ_RESET_VALUE_A" -> readResetValueA,
    "READ_RESET_VALUE_B" -> readResetValueB,
    "RST_MODE_A" -> resetMode(clockDomainA),
    "RST_MODE_B" -> resetMode(clockDomainB),
    "SIM_ASSERT_CHK" -> simAssertChk,
    "USE_EMBEDDED_CONSTRAINT" -> useEmbeddedConstraint,
    "USE_MEM_INIT" -> useMemInit,
    "USE_MEM_INIT_MMI" -> useMemInitMmi,
    "WRITE_DATA_WIDTH_A" -> writeDataWidthA,
  )

  val io = new Bundle {
    val addra = in UInt (addrWidthA bits)
    val addrb = in UInt (addrWidthB bits)
    val clka = in Bool()
    val clkb = in Bool()
    val dina = in Bits (writeDataWidthA bits)
    val douta = out Bits (readDataWidthA bits)
    val doutb = out Bits (readDataWidthB bits)
    val ena = in Bool()
    val enb = in Bool()
    val regcea = in Bool()
    val regceb = in Bool()
    val rsta = in Bool()
    val rstb = in Bool()
    val wea = in Bits (writeDataWidthA / byteWriteWidthA bits)
  }

  mapClockDomain(clockDomainA, io.clka, io.rsta)
  mapClockDomain(clockDomainB, io.clkb, io.rstb)

  noIoPrefix()

  setBlackBoxName("xpm_memory_dpdistram")
}

class XpmMemorySDpRam(clockDomainA: ClockDomain,
                      clockDomainB: ClockDomain,
                      val addrWidthA: Int = 6,
                      val addrWidthB: Int = 6,
                      val autoSleepTime: Int = 0,
                      val useWriteMaskA: Boolean = false,
                      val cascadeHeight: Int = 0,
                      val clockingMode: String = "common_clock",
                      val eccBitRange: String = "7:0",
                      val eccMode: String = "no_ecc",
                      val eccType: String = "none",
                      val ignoreInitSynth: Boolean = false,
                      val memoryInitFile: String = "none",
                      val memoryInitParam: String = "0",
                      val memoryOptimization: String = "true",
                      val memoryPrimitive: String = "auto",
                      val memorySize: Int = 2048,
                      val messageControl: Boolean = false,
                      val readDataWidthB: Int = 32,
                      val readLatencyB: Int = 2,
                      val readResetValueB: String = "0",
                      val simAssertChk: Boolean = false,
                      val useEmbeddedConstraint: Boolean = false,
                      val useMemInit: Boolean = true,
                      val useMemInitMmi: Boolean = false,
                      val wakeUpTime: String = "disable_sleep",
                      val writeDataWidthA: Int = 32,
                      val writeModeB: String = "no_change",
                      val writeProtect: Boolean = true,
                     ) extends BlackBox {
  checkParam(clockingMode)("common_clock", "independent_clock")
  checkParam(eccMode)("no_ecc", "both_encode_and_decode", "decode_only", "encode_only")
  checkParam(eccType)("none", "ECCHSIAO32-7", "ECCHSIAO64-8", "ECCHSIAO128-9", "ECCH32-7", "ECCH64-8")
  checkParam(memoryOptimization)("true", "false")
  checkParam(memoryPrimitive)("auto", "block", "distributed", "mixed", "ultra")
  checkParam(wakeUpTime)("disable_sleep", "use_sleep_pin")
  checkParam(writeModeB)("no_change", "read_first", "write_first")

  val byteWriteWidthA = if (useWriteMaskA) 8 else writeDataWidthA

  addGenerics(
    "ADDR_WIDTH_A" -> addrWidthA,
    "ADDR_WIDTH_B" -> addrWidthB,
    "AUTO_SLEEP_TIME" -> autoSleepTime,
    "BYTE_WRITE_WIDTH_A" -> byteWriteWidthA,
    "CASCADE_HEIGHT" -> cascadeHeight,
    "CLOCKING_MODE" -> clockingMode,
    "ECC_BIT_RANGE" -> eccBitRange,
    "ECC_MODE" -> eccMode,
    "ECC_TYPE" -> eccType,
    "IGNORE_INIT_SYNTH" -> ignoreInitSynth,
    "MEMORY_INIT_FILE" -> memoryInitFile,
    "MEMORY_INIT_PARAM" -> memoryInitParam,
    "MEMORY_OPTIMIZATION" -> memoryOptimization,
    "MEMORY_PRIMITIVE" -> memoryPrimitive,
    "MEMORY_SIZE" -> memorySize,
    "MESSAGE_CONTROL" -> messageControl,
    "READ_DATA_WIDTH_B" -> readDataWidthB,
    "READ_LATENCY_B" -> readLatencyB,
    "READ_RESET_VALUE_B" -> readResetValueB,
    "RST_MODE_A" -> resetMode(clockDomainA),
    "RST_MODE_B" -> resetMode(clockDomainB),
    "SIM_ASSERT_CHK" -> simAssertChk,
    "USE_EMBEDDED_CONSTRAINT" -> useEmbeddedConstraint,
    "USE_MEM_INIT" -> useMemInit,
    "USE_MEM_INIT_MMI" -> useMemInitMmi,
    "WAKEUP_TIME" -> wakeUpTime,
    "WRITE_DATA_WIDTH_A" -> writeDataWidthA,
    "WRITE_MODE_B" -> writeModeB,
    "WRITE_PROTECT" -> writeProtect,
  )

  val io = new Bundle {
    val addra = in UInt (addrWidthA bits)
    val addrb = in UInt (addrWidthB bits)
    val clka = in Bool()
    val clkb = in Bool()
    val dbiterrb = out Bool()
    val dina = in Bits (writeDataWidthA bits)
    val doutb = out Bits (readDataWidthB bits)
    val ena = in Bool()
    val enb = in Bool()
    val injectdbiterra = in Bool()
    val injectsbiterra = in Bool()
    val regceb = in Bool()
    val rstb = in Bool()
    val sbiterrb = out Bool()
    val sleep = in Bool()
    val wea = in Bits (writeDataWidthA / byteWriteWidthA bits)
  }

  mapClockDomain(clockDomainA, io.clka)
  mapClockDomain(clockDomainB, io.clkb, io.rstb)
  noIoPrefix()

  setBlackBoxName("xpm_memory_sdpram")
}

class XpmMemorySpRam(
                      val addrWidthA: Int = 6,
                      val autoSleepTime: Int = 0,
                      val cascadeHeight: Int = 0,
                      val useWriteMaskA: Boolean = false,
                      val clockingMode: String = "common_clock",
                      val eccBitRange: String = "7:0",
                      val eccMode: String = "no_ecc",
                      val eccType: String = "none",
                      val ignoreInitSynth: Boolean = false,
                      val memoryInitFile: String = "none",
                      val memoryInitParam: String = "0",
                      val memoryOptimization: String = "true",
                      val memoryPrimitive: String = "auto",
                      val memorySize: Int = 2048,
                      val messageControl: Boolean = false,
                      val readDataWidthA: Int = 32,
                      val readLatencyA: Int = 2,
                      val readResetValueA: String = "0",
                      val simAssertChk: Boolean = false,
                      val useEmbeddedConstraint: Boolean = false,
                      val useMemInit: Boolean = true,
                      val useMemInitMmi: Boolean = false,
                      val wakeUpTime: String = "disable_sleep",
                      val writeDataWidthA: Int = 32,
                      val writeModeA: String = "no_change",
                      val writeProtect: Boolean = true,
                    ) extends BlackBox {
  checkParam(clockingMode)("common_clock", "independent_clock")
  checkParam(eccMode)("no_ecc", "both_encode_and_decode", "decode_only", "encode_only")
  checkParam(eccType)("none", "ECCHSIAO32-7", "ECCHSIAO64-8", "ECCHSIAO128-9", "ECCH32-7", "ECCH64-8")
  checkParam(memoryOptimization)("true", "false")
  checkParam(memoryPrimitive)("auto", "block", "distributed", "mixed", "ultra")
  checkParam(wakeUpTime)("disable_sleep", "use_sleep_pin")
  checkParam(writeModeA)("no_change", "read_first", "write_first")

  val byteWriteWidthA = if (useWriteMaskA) 8 else writeDataWidthA

  addGenerics(
    "ADDR_WIDTH_A" -> addrWidthA,
    "AUTO_SLEEP_TIME" -> autoSleepTime,
    "BYTE_WRITE_WIDTH_A" -> byteWriteWidthA,
    "CASCADE_HEIGHT" -> cascadeHeight,
    "CLOCKING_MODE" -> clockingMode,
    "ECC_BIT_RANGE" -> eccBitRange,
    "ECC_MODE" -> eccMode,
    "ECC_TYPE" -> eccType,
    "IGNORE_INIT_SYNTH" -> ignoreInitSynth,
    "MEMORY_INIT_FILE" -> memoryInitFile,
    "MEMORY_INIT_PARAM" -> memoryInitParam,
    "MEMORY_OPTIMIZATION" -> memoryOptimization,
    "MEMORY_PRIMITIVE" -> memoryPrimitive,
    "MEMORY_SIZE" -> memorySize,
    "MESSAGE_CONTROL" -> messageControl,
    "READ_DATA_WIDTH_A" -> readDataWidthA,
    "READ_LATENCY_A" -> readLatencyA,
    "READ_RESET_VALUE_A" -> readResetValueA,
    "RST_MODE_A" -> resetMode(clockDomain),
    "SIM_ASSERT_CHK" -> simAssertChk,
    "USE_EMBEDDED_CONSTRAINT" -> useEmbeddedConstraint,
    "USE_MEM_INIT" -> useMemInit,
    "USE_MEM_INIT_MMI" -> useMemInitMmi,
    "WAKEUP_TIME" -> wakeUpTime,
    "WRITE_DATA_WIDTH_A" -> writeDataWidthA,
    "WRITE_MODE_A" -> writeModeA,
    "WRITE_PROTECT" -> writeProtect,
  )

  val io = new Bundle {
    val addra = in UInt (addrWidthA bits)
    val clka = in Bool()
    val dbiterra = out Bool()
    val dina = in Bits (writeDataWidthA bits)
    val douta = in Bits (readDataWidthA bits)
    val ena = in Bool()
    val injectdbiterra = in Bool()
    val injectsbiterra = in Bool()
    val regcea = in Bool()
    val rsta = in Bool()
    val sbiterra = out Bool()
    val sleep = in Bool()
    val wea = in Bits (writeDataWidthA / byteWriteWidthA bits)
  }

  mapCurrentClockDomain(io.clka, io.rsta)

  noIoPrefix()

  setBlackBoxName("xpm_memory_spram")
}

class XpmMemoryTDpRam(
                       clockDomainA: ClockDomain,
                       clockDomainB: ClockDomain,
                       val addrWidthA: Int = 6,
                       val addrWidthB: Int = 6,
                       val autoSleepTime: Int = 0,
                       val useWriteMaskA: Boolean = false,
                       val useWriteMaskB: Boolean = false,
                       val cascadeHeight: Int = 0,
                       val clockingMode: String = "common_clock",
                       val eccBitRange: String = "7:0",
                       val eccMode: String = "no_ecc",
                       val eccType: String = "none",
                       val ignoreInitSynth: Boolean = false,
                       val memoryInitFile: String = "none",
                       val memoryInitParam: String = "0",
                       val memoryOptimization: String = "true",
                       val memoryPrimitive: String = "auto",
                       val memorySize: Int = 2048,
                       val messageControl: Boolean = false,
                       val readDataWidthA: Int = 32,
                       val readDataWidthB: Int = 32,
                       val readLatencyA: Int = 2,
                       val readLatencyB: Int = 2,
                       val readResetValueA: String = "0",
                       val readResetValueB: String = "0",
                       val simAssertChk: Boolean = false,
                       val useEmbeddedConstraint: Boolean = false,
                       val useMemInit: Boolean = true,
                       val useMemInitMmi: Boolean = false,
                       val wakeUpTime: String = "disable_sleep",
                       val writeDataWidthA: Int = 32,
                       val writeDataWidthB: Int = 32,
                       val writeModeA: String = "no_change",
                       val writeModeB: String = "no_change",
                       val writeProtect: Boolean = true,
                     ) extends BlackBox {
  checkParam(clockingMode)("common_clock", "independent_clock")
  checkParam(eccMode)("no_ecc", "both_encode_and_decode", "decode_only", "encode_only")
  checkParam(eccType)("none", "ECCHSIAO32-7", "ECCHSIAO64-8", "ECCHSIAO128-9", "ECCH32-7", "ECCH64-8")
  checkParam(memoryOptimization)("true", "false")
  checkParam(memoryPrimitive)("auto", "block", "distributed", "mixed", "ultra")
  checkParam(wakeUpTime)("disable_sleep", "use_sleep_pin")
  checkParam(writeModeA)("no_change", "read_first", "write_first")
  checkParam(writeModeB)("no_change", "read_first", "write_first")

  val byteWriteWidthA = if (useWriteMaskA) 8 else writeDataWidthA
  val byteWriteWidthB = if (useWriteMaskB) 8 else writeDataWidthB

  addGenerics(
    "ADDR_WIDTH_A" -> addrWidthA,
    "ADDR_WIDTH_B" -> addrWidthB,
    "AUTO_SLEEP_TIME" -> autoSleepTime,
    "BYTE_WRITE_WIDTH_A" -> byteWriteWidthA,
    "BYTE_WRITE_WIDTH_B" -> byteWriteWidthB,
    "CASCADE_HEIGHT" -> cascadeHeight,
    "CLOCKING_MODE" -> clockingMode,
    "ECC_BIT_RANGE" -> eccBitRange,
    "ECC_MODE" -> eccMode,
    "ECC_TYPE" -> eccType,
    "IGNORE_INIT_SYNTH" -> ignoreInitSynth,
    "MEMORY_INIT_FILE" -> memoryInitFile,
    "MEMORY_INIT_PARAM" -> memoryInitParam,
    "MEMORY_OPTIMIZATION" -> memoryOptimization,
    "MEMORY_PRIMITIVE" -> memoryPrimitive,
    "MEMORY_SIZE" -> memorySize,
    "MESSAGE_CONTROL" -> messageControl,
    "READ_DATA_WIDTH_A" -> readDataWidthA,
    "READ_DATA_WIDTH_B" -> readDataWidthB,
    "READ_LATENCY_A" -> readLatencyA,
    "READ_LATENCY_B" -> readLatencyB,
    "READ_RESET_VALUE_A" -> readResetValueA,
    "READ_RESET_VALUE_B" -> readResetValueB,
    "RST_MODE_A" -> resetMode(clockDomainA),
    "RST_MODE_B" -> resetMode(clockDomainB),
    "SIM_ASSERT_CHK" -> simAssertChk,
    "USE_EMBEDDED_CONSTRAINT" -> useEmbeddedConstraint,
    "USE_MEM_INIT" -> useMemInit,
    "USE_MEM_INIT_MMI" -> useMemInitMmi,
    "WAKEUP_TIME" -> wakeUpTime,
    "WRITE_DATA_WIDTH_A" -> writeDataWidthA,
    "WRITE_DATA_WIDTH_B" -> writeDataWidthB,
    "WRITE_MODE_A" -> writeModeA,
    "WRITE_MODE_B" -> writeModeB,
    "WRITE_PROTECT" -> writeProtect,
  )

  val io = new Bundle {
    val addra = in UInt (addrWidthA bits)
    val addrb = in UInt (addrWidthB bits)
    val clka = in Bool()
    val clkb = in Bool()
    val dbiterra = out Bool()
    val dbiterrb = out Bool()
    val dina = in Bits (writeDataWidthA bits)
    val dinb = in Bits (writeDataWidthB bits)
    val douta = out Bits (readDataWidthA bits)
    val doutb = out Bits (readDataWidthB bits)
    val ena = in Bool()
    val enb = in Bool()
    val injectdbiterra = in Bool()
    val injectdbiterrb = in Bool()
    val injectsbiterra = in Bool()
    val injectsbiterrb = in Bool()
    val regcea = in Bool()
    val regceb = in Bool()
    val rsta = in Bool()
    val rstb = in Bool()
    val sbiterra = out Bool()
    val sbiterrb = out Bool()
    val sleep = in Bool()
    val wea = in Bits (writeDataWidthA / byteWriteWidthA bits)
    val web = in Bits (writeDataWidthB / byteWriteWidthB bits)
  }

  mapClockDomain(clockDomainA, io.clka, io.rsta)
  mapClockDomain(clockDomainB, io.clkb, io.rstb)

  noIoPrefix()

  setBlackBoxName("xpm_memory_tdpram")
}
