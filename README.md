# TC-evaluator

A tree calculus evaluator library written in C.

## Description

Tree Calculus (TC) is a mathematical model of computation (see [Acknowledgements](#acknowledgements)), similar to Lambda Calculus, but with introspection capabilities. TC-evaluator is a virtual machine that reduces trees according to TC's reduction rules. It has no IO or side effects.

It is not meant to be used directly, but as a language backend, like the STG-machine for GHC, or the BEAM VM for Erlang.

## How to Build

Use cmake to build the lib:
```
cmake -B build
cmake --build build
```

If you also want to build the example:
```
cmake -B build
cmake --build build --target tceval_example
```

The default build mode is Release (which can also be set with `cmake -B build -DCMAKE_BUILD_TYPE=Release`). You can build with profiling info with `cmake -B build -DCMAKE_BUILD_TYPE=Profiling`.

## Usage

The example at `examples/main.c` shows a basic way of using this lib. It shows how to:

- Create, save and load a VM
- Run the reduction process
- Query the resulting structure

The comments give more information for each operation.

## Acknowledgements

Tree Calculus was discovered by [Barry Jay](https://github.com/barry-jay-personal/blog).

More information and an interactive playground can be found at [treecalcul.us](https://treecalcul.us), a website made by [Johannes Bader](https://johannes-bader.com/).
