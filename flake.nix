{
  description = "aoc-2022 flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "aarch64-darwin"; };

      roc = pkgs.stdenv.mkDerivation {
        pname = "roc";
        version = "0.0.1";
        src = pkgs.fetchurl {
          # nix-prefetch-url this URL to find the hash value
          url = "https://github.com/roc-lang/roc/releases/download/nightly/roc_nightly-macos_12_apple_silicon-2022-12-07-2ec473b.tar.gz";
          sha256 = "1zsna63p3zvgbgkkwgfnfm58nf2fy3r25pf4zpnjxmmn1izjh26d";
        };
        phases = [ "installPhase" ];
        installPhase = ''
          mkdir -p $out/bin
          cd $out/bin && tar -zxf $src
        '';
      };

    in {
      devShell.aarch64-darwin = pkgs.mkShell {
        buildInputs = [
          roc
        ];
      };
    };
}
