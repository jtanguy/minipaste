{ mkDerivation, base, blaze-html, bytestring, either, hasql
, hasql-postgres, highlighting-kate, http-types, mtl, old-locale
, servant, servant-blaze, servant-server, stdenv, template-haskell
, text, time, transformers, uuid, wai, warp
}:
mkDerivation {
  pname = "minipaste";
  version = "0.6.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base blaze-html bytestring either hasql hasql-postgres
    highlighting-kate http-types mtl old-locale servant servant-blaze
    servant-server template-haskell text time transformers uuid wai
    warp
  ];
  homepage = "https://github.com/jtanguy/minipaste";
  description = "Minimal pastebin-like service";
  license = stdenv.lib.licenses.bsd3;
}
