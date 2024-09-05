{
  inputs.nixpkgs.url = "nixpkgs";

  outputs = {nixpkgs, ...}: let
    systems = ["x86_64-linux"];
    perSystem = f: nixpkgs.lib.genAttrs systems (s: f nixpkgs.legacyPackages.${s});
  in {
    devShells = perSystem (pkgs: {
      default = pkgs.mkShell {
        buildInputs = with pkgs; [zig];
      };
    });
  };
}
