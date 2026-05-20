{
  description = "Hegel for OCaml";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # note: this version is automatically bumped when we update hegel-core, do not update manually
    hegel.url = "git+https://github.com/hegeldev/hegel-core?dir=nix&ref=refs/tags/v0.9.1"; # git+https instead of github so that we can use the ref parameter
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
      lib.mkHegelOcamlProject =
        {
          pkgs,
          stdenv ? pkgs.stdenv,
        }:
        let
          duneProjectLines = builtins.filter builtins.isString (
            builtins.split "\n" (builtins.readFile ../dune-project)
          );
          versionLine = builtins.head (
            builtins.filter (l: builtins.match "\\(version .*\\)" l != null) duneProjectLines
          );
          duneProjectVersion = builtins.elemAt (builtins.match "\\(version ([^)]+)\\)" versionLine) 0;
        in
        pkgs.ocamlPackages.buildDunePackage {
          pname = "hegel";
          version = duneProjectVersion;
          src = ../.;
          duneVersion = "3";
          propagatedBuildInputs = with pkgs.ocamlPackages; [
            core
            core_unix
            ocplib-endian
            ppx_js_style
            ppxlib
            yojson
          ];
          buildPhase = ''
            runHook preBuild
            dune build -p hegel,ppx_hegel_test,ppx_hegel_generator -j $NIX_BUILD_CORES
            runHook postBuild
          '';
          installPhase = ''
            runHook preInstall
            dune install --prefix $out --libdir $OCAMLFIND_DESTDIR \
              hegel ppx_hegel_test ppx_hegel_generator
            runHook postInstall
          '';
          doCheck = false;
        };

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
