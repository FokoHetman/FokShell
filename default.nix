{ pkgs ? import <nixpkgs> { }, ... }:
let
  mf = (pkgs.lib.importTOML ./Cargo.toml).package;
  config = import ./configuration.nix;
in
  pkgs.rustPlatform.buildRustPackage rec {
    pname = mf.name;
    version = mf.version;
    src = pkgs.lib.cleanSource ./.;

    buildInputs = with pkgs; [
      libclang
    ];
    PKG_CONFIG_PATH = "${pkgs.openssl.dev}/lib/pkgconfig";
    LIBCLANG_PATH = "${pkgs.llvmPackages_12.libclang.lib}/lib";
    cargoLock.lockFile = ./Cargo.lock;
    
    #cargoSha256 = nixpkgs.lib.fakeSha256;
}
