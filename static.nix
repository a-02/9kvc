{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc944", doBenchmark ? false }:

let

  pkgs = nixpkgs.pkgsMusl;

  ninek = { mkDerivation, arithmoi, base, bv, bytestring, directory, hspec
      , http-media, lib, lucid, network, scotty, servant, servant-server
      , sqlite-simple, text, time, wai, wai-middleware-static, warp, random
      }:
      mkDerivation {
        pname = "9k";
        version = "0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base ];
        executableHaskellDepends = [
          arithmoi base bv bytestring directory http-media lucid network
          scotty sqlite-simple text time wai
          wai-middleware-static warp random
        ];
        license = lib.licenses.bsd3;
        enableSharedExecutables = false;
        enableSharedLibraries = false;
        configureFlags = [
          "--ghc-option=-optl=-static"
          "--ghc-option=-optl=-pthread"
          "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
          "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
          "--ghc-option=-optl=-L${pkgs.musl}/lib"
          "--ghc-option=-optl=-L${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
        ];
        mainProgram = "9k";
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage ninek {}; 

in

  if pkgs.lib.inNixShell then drv.env else drv
