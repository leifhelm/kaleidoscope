{ pkgs ? import <nixpkgs> {
#   overlays = [
#     (import "${
#       fetchTarball
#         "https://github.com/nix-community/fenix/archive/main.tar.gz"
#     }/overlay.nix")
#   ];
}
, devTools ? true
}:
with pkgs;
stdenv.mkDerivation {
  name = "env";
  nativeBuildInputs = [ pkg-config ];
  buildInputs = [
    # (fenix.complete.withComponents [
    #   "cargo"
    #   "clippy"
    #   "rust-src"
    #   "rustc"
    #   "rustfmt"
    # ])
    # rust-analyzer-nightly
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
