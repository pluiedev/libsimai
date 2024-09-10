{
  inputs = {
    nixpkgs.url = "nixpkgs";
    zig = {
      url = "github:mitchellh/zig-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {nixpkgs, zig, ...}: let
    systems = ["x86_64-linux"];
    perSystem = f: nixpkgs.lib.genAttrs systems (s: f nixpkgs.legacyPackages.${s});
  in {
    devShells = perSystem (pkgs: {
      default = pkgs.mkShell {
        buildInputs = [zig.packages.${pkgs.system}.master];
      };
    });
  };
}
