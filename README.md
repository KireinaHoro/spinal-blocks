# Building blocks for SpinalHDL designs

This repository contains some useful constructs I use to build hardware in SpinalHDL.  Significant pieces include:

- [Register allocator](blocks/jsteward/blocks/misc/RegAllocator.scala) with blocks management and C header generation
- [Profiler](blocks/jsteward/blocks/misc/Profiling.scala) for taking timestamps and passing them around in the design
- [`verilog-axi` blackboxes](blocks/jsteward/blocks/axi) for the [Verilog AXI](https://github.com/alexforencich/verilog-axi) IP library
- [ECI definitions](blocks/jsteward/blocks/eci) for various definitions for the [ECI toolkit](https://gitlab.inf.ethz.ch/project-openenzian/fpga-stack/eci-toolkit/-/blob/master/hdl/eci_cmd_defs.sv) and using the [Directory Controller Slice](https://gitlab.inf.ethz.ch/project-openenzian/fpga-stack/directory-controller-slice) with Enzian.
