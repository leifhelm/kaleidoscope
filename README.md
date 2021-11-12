# Kaleidoscope compiler

This project implements part of the "Kaleidoscope" language from the [LLVM tutorial](https://llvm.org/docs/tutorial/)

## Build instructions

### Nix with flakes

To just build the package run:
```bash

nix build .

```

To enter the development environment and then build kaleidscope run:
```bash

nix develop
cargo build --release

```

Note: if you have nix-direnv you just have to run `cargo build --release`.

### Linux

Make shure that you have the following dependencies installed:
* LLVM-13
* libffi
* libxml2

And the simply build by executing
```bash

cargo build --release

```
