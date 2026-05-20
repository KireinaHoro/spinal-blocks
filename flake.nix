{
  description = "devShell for KireinaHoro's SpinalHDL blocks";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
  with builtins;
  with nixpkgs.lib;
  flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" ] (system: let
    pkgs = import nixpkgs { inherit system; };

    isSpinal = f: lists.any f.hasExt [
      "scala" "java" "xml" "conf" # spinalhdl
      "mill"                      # mill build files
      "v" "sv"                    # RTL dependencies
    ];
    allSpinalIn = allSourcesIn isSpinal;
  in {
    # for interactive development
    devShells.default = with pkgs; mkShell {
      buildInputs = [
        zlib.dev verilator clang
        gtkwave sby yices
        jdk mill
      ];
      shellHook = ''
        export XDG_CACHE_HOME=$PWD/out/xdg-cache-home/
      '';
    };
  });
}
