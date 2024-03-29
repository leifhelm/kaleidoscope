# Any copyright is dedicated to the Public Domain.
# https://creativecommons.org/publicdomain/zero/1.0/

{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = nixpkgs.legacyPackages.${system};
            customBuildRustCrateForPkgs = pkgs: pkgs.buildRustCrate.override {
              defaultCrateOverrides = pkgs.defaultCrateOverrides // {
                llvm-sys = attrs: {
                  buildInputs = [ pkgs.llvmPackages_13.libllvm.dev ];
                };
                kaleidoscope = attrs: {
                  nativeBuildInputs = [ pkgs.pkg-config ];
                  buildInputs = [
                    pkgs.libffi.dev
                    pkgs.libxml2.dev
                    pkgs.ncurses.dev
                  ];
                };
              };
            };
            cargo_nix = import ./Cargo.nix {
              inherit pkgs;
              buildRustCrateForPkgs = customBuildRustCrateForPkgs;
            };
        in
          rec {
            devShell = import ./shell.nix { inherit pkgs; };
            devShells."cargo" = import ./shell.nix { inherit pkgs; devTools = false; };
            defaultPackage = packages.kaleidoscope;
            packages.kaleidoscope = cargo_nix.workspaceMembers."kaleidoscope".build;
          }
      );
}
