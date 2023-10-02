{ pkgs ? import <nixpkgs> {}, unstable ? import <nixos-unstable> {} }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    zig_0_10
    zls
  ];
}