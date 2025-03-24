{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      perSystem = { self', pkgs, ... }: {
        packages.default = pkgs.stdenvNoCC.mkDerivation rec {
          name = "DaoFP.pdf";
          src = self;
          buildInputs = [
            pkgs.coreutils
            (pkgs.texlive.combine {
              inherit (pkgs.texlive) scheme-full latex-bin latexmk pygmentex;
            })
            pkgs.python37Packages.pygments
            pkgs.which
          ];
          phases = [ "unpackPhase" "buildPhase" "installPhase" ];
          buildPhase = ''
            set -e
            export PATH="${pkgs.lib.makeBinPath buildInputs}";
            # Due to quirks of TeX, we must run this *twice* to 
            # get the ToC to generate.
            pdflatex -shell-escape DaoFP.tex
            pdflatex -shell-escape DaoFP.tex
          '';
          installPhase = ''
            mkdir -p $out
            cp DaoFP.pdf $out/
          '';
        };
      };
    };
}
