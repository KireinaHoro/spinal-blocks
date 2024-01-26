# Building blocks for SpinalHDL designs

This repository contains some useful constructs I use to build hardware in SpinalHDL.  Significant pieces include:

- [Register allocator](blocks/misc/RegAllocator.scala) with blocks management and C header generation
- [`verilog-axi` blackboxes](blocks/blackbox/axi) for the [Verilog AXI](https://github.com/alexforencich/verilog-axi) IP library
