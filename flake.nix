{
  description = "halogen-infinite-scroll";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { nixpkgs, purescript-overlay, ... }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    in {
      devShells = nixpkgs.lib.genAttrs systems (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ purescript-overlay.overlays.default ];
          };
        in {
          default = pkgs.mkShell {
            packages = with pkgs; [ nodejs_22 purs spago purescript-language-server purs-tidy esbuild ];
          };
        });
    };
}
