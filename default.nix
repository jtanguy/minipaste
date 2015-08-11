{ mkDerivation, base, blaze-html, bytestring, hasql, hasql-postgres
, highlighting-kate, http-types, mtl, old-locale, scotty, stdenv
, text, time, transformers, uuid, wai
}:
mkDerivation {
  pname = "minipaste";
  version = "0.5.5";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base blaze-html bytestring hasql hasql-postgres highlighting-kate
    http-types mtl old-locale scotty text time transformers uuid wai
  ];
  homepage = "https://github.com/jtanguy/minipaste";
  description = "Minimal pastebin-like service";
  license = stdenv.lib.licenses.bsd3;
}
