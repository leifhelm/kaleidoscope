{ pkgs ? import <nixpkgs> { } }:
with pkgs;
stdenv.mkDerivation {
  name = "env";
  nativeBuildInputs = [ pkg-config ];
  buildInputs = [
    cargo
    rustc
    crate2nix
    (llvmPackages_12.libllvm.override {
      enableSharedLibraries = false;
    }).dev
    libffi.dev
    libxml2.dev
  ];
}
