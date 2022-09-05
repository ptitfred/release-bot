{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc902" }:
let releaseBot = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./release-bot.nix { };
 in releaseBot.overrideAttrs (oldAttrs: {
      buildInputs = oldAttrs.buildInputs or [] ++ [ nixpkgs.coreutils ];
      postInstall = oldAttrs.postInstall or "" + ''
        mkdir -p $out/share/assets
        cp -R $src/public/* $out/share/assets/
      '';
    })
