# Scribble

Many projects attempt to write replacements or iterations of the venerable `C`
programming language. This project is no different, in the sense that it will
do it at least as poorly as the majority of these projects.

### Why?

See [![justforfunnoreally.dev badge](https://img.shields.io/badge/justforfunnoreally-dev-9ff)](https://justforfunnoreally.dev)
and the [Obelix Manifesto](Manifesto.md).

## Goals

### General

- [ ] A compiler for a `C`-like language.
- [ ] Initially target the `aarch64` architecture on Apple Silicon and the Raspberry Pi 4.
- [ ] Allow linking with existing `C` libraries and allow `C` programs to link with `scribble` libraries.
- [ ] Follow the [Obelix Manifesto](Manifesto.md) as much as possible.

### Improvements on `C`:

- [ ] Polymorphic functions. There are many in my opinion poor features in `C++`, but polymorphism is good.
- [ ] `Rust`-like `enum`s and `match` statements.
- [ ] Some sort of objects and methods. Methods will most likely be inferred and as such be closer to syntactic sugar
  than an actual part of the type system.
- [ ] Lambdas and closures of some sort.
- [ ] Functional programming features: list comprehensions and reductions, mappings.

### No, not doing it:

- Inheritance

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
