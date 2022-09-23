{
  inputs = {
    opam-repository.url = "github:ocaml/opam-repository";
    opam-repository.flake = false;

    # 5.0 patches
    opam-repository-alpha.url = "github:kit-ty-kate/opam-alpha-repository";
    opam-repository-alpha.flake = false;

    # Unlocks 5.0 on the main repository
    opam-repository-beta.url = "github:ocaml/ocaml-beta-repository";
    opam-repository-beta.flake = false;

    opam-nix.url = "github:tweag/opam-nix";
    opam-nix.inputs.nixpkgs.follows = "nixpkgs";
    opam-nix.inputs.opam-repository.follows = "opam-repository";

    utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self, utils, opam-nix, nixpkgs,
    opam-repository, opam-repository-alpha, opam-repository-beta,
  }:
    let
      package = "scrutiny";
      repos = [
        # opam-repository-alpha
        opam-repository-beta
        opam-repository
      ];

      ocamlOverlay = final: prev: {
        # https://github.com/tweag/opam-nix/issues/21
        # ocaml = prev.ocaml.overrideAttrs(oa: {
        #   nativeBuildInputs = oa.nativeBuildInputs ++ [final.ocaml-base-compiler];
        # });

        # Force a version with the hashes present
        base = prev.base.overrideAttrs(oa: {
          src = builtins.fetchTarball {
            url = "https://github.com/kit-ty-kate/base/archive/500-015.tar.gz";
            sha256 = "15vsiv3q53l1bzrvqgspf3lp2104s9dzw62z3nl75f53jvjvsyf6";
          };
        });
      };
    in utils.lib.eachDefaultSystem (system:
      let
        opam = opam-nix.lib.${system};
        pkgs = import nixpkgs { inherit system; };

        project = opam.buildOpamProject {
          inherit pkgs repos;

          resolveArgs = { dev = false; };
          overlays = [ocamlOverlay opam.defaultOverlay];
        } package ./. {
          ocaml-base-compiler = "*";

          # The alpha repository is missing hashes, and so doesn't play well
          # with flakes. Force an older version of ppx_deriving.
          ppx_deriving = "5.2.1";
        };
        scrutiny = project.${package}.overrideAttrs(oa: {
          buildInputs = oa.buildInputs ++ [pkgs.systemd];
          nativeBuildInputs = oa.nativeBuildInputs ++ [pkgs.upx];

          removeOcamlReferences = true;
          propagateInputs = false;
        });

        mkSingleExe = { name, exe ? name }: pkgs.stdenv.mkDerivation {
          inherit name;
          inherit (scrutiny) version;

          unpackPhase = ":";
          installPhase = ''
            mkdir -p $out/bin
            cp "${scrutiny}/bin/${exe}" "$out/bin/${exe}"
          '';
        };
      in {
        inherit opam;
        packages.default = scrutiny;
        packages.scrutiny-infra-tunnel = mkSingleExe { name = "scrutiny-infra-tunnel"; };
        packages.scrutiny-systemd-exporter = mkSingleExe { name = "scrutiny-systemd-exporter"; exe = "systemd_exporter"; };
      }) // { inherit repos; };
}
