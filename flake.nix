{
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = nixpkgs.legacyPackages.${system};
            llvm = (pkgs.llvmPackages_12.libllvm.override {
              enableSharedLibraries = false;
            }).dev;
            customBuildRustCrateForPkgs = pkgs: pkgs.buildRustCrate.override {
              defaultCrateOverrides = pkgs.defaultCrateOverrides // {
                llvm-sys = attrs: {
                  buildInputs = [ llvm ];
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
            defaultPackage = packages.kaleidoscope;
            packages.kaleidoscope = cargo_nix.workspaceMembers."kaleidoscope".build;
          }
      );
}
