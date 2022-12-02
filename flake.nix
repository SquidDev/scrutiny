{
  inputs = {
    opam-repository.url = "github:ocaml/opam-repository";
    opam-repository.flake = false;

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
    opam-repository, opam-repository-beta,
  }:
    let
      package = "scrutiny";
      repos = [
        opam-repository-beta
        opam-repository
      ];
      overlay = pkgs: _: let
        opam = opam-nix.lib.${pkgs.system};
        project = opam.buildOpamProject {
          inherit pkgs repos;

          resolveArgs = { dev = false; };
          overlays = [opam.defaultOverlay];
        } package ./. {};
        scrutiny = project.${package}.overrideAttrs(oa: {
          buildInputs = oa.buildInputs ++ [pkgs.systemdMinimal];
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
        inherit scrutiny;
        scrutiny-infra-tunnel = mkSingleExe { name = "scrutiny-infra-tunnel"; };
        scrutiny-systemd-exporter = mkSingleExe { name = "scrutiny-systemd-exporter"; exe = "systemd_exporter"; };
      };
    in {
      inherit overlay;
    } // utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; overlays = [overlay]; }; in {
        packages.default = pkgs.scrutiny;
        packages.scrutiny-infra-tunnel = pkgs.scrutiny-infra-tunnel;
        packages.scrutiny-systemd-exporter = pkgs.scrutiny-systemd-exporter;
      });
}
