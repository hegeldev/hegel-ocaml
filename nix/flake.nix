{
  description = "Hegel for OCaml";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # note: this version is automatically bumped when we update hegel-core, do not update manually
    hegel.url = "git+https://github.com/hegeldev/hegel-core?dir=nix&ref=refs/tags/v0.4.0"; # git+https instead of github so that we can use the ref parameter
    flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";
  };

  outputs =
    {
      self,
      nixpkgs,
      hegel,
      ...
    }:
    let
      forAllSystems = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;
    in
    {
      devShells = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.mkShell {
            packages = [
              pkgs.ocaml
              pkgs.dune_3
              pkgs.opam
              pkgs.just
              pkgs.uv
            ];
            HEGEL_SERVER_COMMAND = pkgs.lib.getExe hegel.packages.${system}.default;
          };
        }
      );
    };
}
