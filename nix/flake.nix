{
  description = "Hegel for OCaml";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";
  };

  outputs =
    {
      self,
      nixpkgs,
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
            ctypes
            ctypes-foreign
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
              # curl is used by the loader's download fallback (see below).
              pkgs.curl
            ];
            # The native engine (libhegel) is resolved at runtime by
            # lib/ffi/loader.ml, not provided by Nix: it checks
            # $HEGEL_LIBHEGEL_PATH, then a sibling
            # ../hegel-rust/target/{release,debug}/ checkout, then a
            # SHA-256-verified download cached under ~/.cache/hegel-ocaml.
            # Build hegel-rust locally and set HEGEL_LIBHEGEL_PATH to skip the
            # download (or rely on the sibling-checkout lookup).
          };
        }
      );
    };
}
