# Any copyright is dedicated to the Public Domain.
# https://creativecommons.org/publicdomain/zero/1.0/

{ pkgs ? import <nixpkgs> { }
, devTools ? true
}:
with pkgs;
stdenv.mkDerivation {
  name = "env";
  nativeBuildInputs = [ pkg-config ];
  buildInputs = [
    cargo
    llvmPackages_13.libllvm.dev
    libffi.dev
    libxml2.dev
  ] ++ lib.optionals devTools ([
    rustc
    crate2nix
    gdb
  ]  ++ lib.optionals (stdenv.isx86_64 && stdenv.isLinux) [
    rr
  ]);
}
