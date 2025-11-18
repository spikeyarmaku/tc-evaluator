# TC-evaluator

#### A tree calculus evaluator library written in C

TC-evaluator is a virtual machine that reduces trees according to tree calculus' reduction rules. It has no IO or side effects.

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

The example at `examples/main.c` shows a basic way of using this lib.

## Acknowledgements

Tree Calculus was discovered by [Barry Jay](https://github.com/barry-jay-personal/blog).

[treecalcul.us](https://treecalcul.us) is an excellent website with an intuitive Tree Calculus code playground created by [Johannes Bader](https://johannes-bader.com/).
