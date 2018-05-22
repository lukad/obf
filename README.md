# obf

`obf` is a Brainfuck interpreter using [Llvm](https://llvm.org/).

## Demo

[![asciicast](https://asciinema.org/a/179731.png)](https://asciinema.org/a/179731)

## Installing

### Prerequesites

Install [OCaml](https://ocaml.org/) and [OPAM](https://opam.ocaml.org/).

### Building from source

```bash
git clone git@github.com:lukad/obf.git
cd obf
opam pin add obf $PWD -n
opam update
opam depext --install obf
```
