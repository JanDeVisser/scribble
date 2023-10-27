# Scribble

Many projects attempt to write replacements or iterations of the venerable `C`
programming language. This project is no different, in the sense that it will
do it at least as poorly as the majority of these projects.

## Goals

- [ ] A compiler for a `C`-like language.
- [ ] Initially target the `aarch64` architecture on Apple Silicon and the Raspberry Pi 4.
- [ ] Allow linking with existing `C` libraries and allow `C` programs to link with `scribble` libraries.
- [ ] Follow the [Obelix Manifesto](Manifesto.md) as much as possible.

Improvements on `C`:

- [ ] Polymorphic functions. There are many in my opinion poor features in `C++`, but polymorphism is good.
- [ ] `Rust`-like `enum`s and `match` statements.
- [ ] Lambdas and closures of some sort.

## Installation

```shell
$ cd ~/projects
$ git clone git@github.com:JanDeVisser/scribble.git
$ cd scribble
$ mkdir build
$ cd build
$ cmake ..
$ cmake --build .
$ cmake --install .
$ bin/scribble test/scribble/hello.scribble 
```
