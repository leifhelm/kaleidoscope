{ pkgs ? import <nixpkgs> {
#   overlays = [
#     (import "${
#       fetchTarball
#         "https://github.com/nix-community/fenix/archive/main.tar.gz"
#     }/overlay.nix")
#   ];
}
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
    rustc
    crate2nix
    rr
    gdb
    llvmPackages_13.libllvm.dev
    libffi.dev
    libxml2.dev
  ];
}
