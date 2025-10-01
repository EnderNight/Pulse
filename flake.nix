{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.mkShell {
        name = "pulse";
        packages = (with pkgs; [
          ocaml
          dune_3

          gcc15
          gnumake
          bear

          qbe
        ]) ++ (with pkgs.ocamlPackages; [
          findlib

          ocaml-lsp
          ocamlformat
          odoc
          utop
        ]);
      };
    };
}
