**Contacts for Support**
- @rupertlssmith on https://elmlang.slack.com
- @rupert on https://discourse.elm-lang.org

# Release Notes

1.0.0 - A fairly rough and ready MLIR model and pretty printer. Has been used succesfully to compile the Toy
tutorial and generate a binary executable from the output.

# What is MLIR?

MLIR (Multi-Level Intermediate Representation Overview) is a compiler IR that is modular and extensible
and aimed at making writing compiler backends easier and with re-usable resources from many authors. It grew
out of the LLVM (Low-Level Virtual Machine) compiler and toolchain project.

In MLIR there are many so-called "dialects" which are modular mini languages within MLIR. In fact LLVM itself
is an MLIR dialect. An MLIR dialect can be thought of as a re-usable module of code for building compiler 
backends and generating efficient code. In common with LLVM, MLIR is in SSA (Single Static Assignment) form, 
in which every variable assignment is immutable, making reasoning about the correctness of optimizations more
possible. Where LLVM has a fixed instruction set, MLIR can be extended with new dialects, allowing its
instruction set to grow.

MLIR is written in C++.

# elm-mlir

This elm-mlir package is a DSL that allows you to build a model of MLIR code in Elm. 
 
That model can be printed to a text file for futher processing by other MLIR tools.

This Elm package does not (yet) provide a complete language binding onto MLIR. One difficulty in implementing
a language binding as opposed to going through the text file to get from Elm to the C++ MLIR toolchain, is that
there is no simple FFI from Elm into C++. It would be possible to write a NodeJS plugin module with C++ 
implementation that Elm could invoke via ports and this is a future aim of this project.