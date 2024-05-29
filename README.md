# fluoresce

[![Package Version](https://img.shields.io/hexpm/v/fluoresce)](https://hex.pm/packages/fluoresce)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/fluoresce/)

An algebraic effects framework for Gleam, with classical-logic types. 
This is could also be seen as a library for the Continuation monad.

This is very Haskell-y, and is based directly on the paper "The Duality of Abstraction," currently marked as a draft by Vikraman Choudhury but which I think is part of POPL 2024. I intend to add more effects features based on the Doo-Bee-Doo-Bee-Doo paper by Conor McBride and others; we'll see how far I get.

This kind of library is not how Gleam does things, as a language. I wrote it for fun. You should be a pretty big fan of control effects before attempting to build a serious project with `fluoresce`!

```sh
gleam add fluoresce
```
```gleam
import fluoresce

pub fn main() {
  // TODO: An example of the project in use
}
```

Further documentation can be found at <https://hexdocs.pm/fluoresce>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
gleam shell # Run an Erlang shell
```
