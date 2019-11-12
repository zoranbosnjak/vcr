{ mkDerivation, array, base, double-conversion, fetchgit, ghc-prim
, integer-gmp, old-locale, stdenv, text, time, transformers
}:
mkDerivation {
  pname = "text-format";
  version = "0.3.1.1";
  src = fetchgit {
    url = "https://github.com/ababkin/text-format";
    sha256 = "1m2ypy6bv2irr0qwn9xmgfhb4axz3nv5g9i3mjslffjamrjybbci";
    rev = "142dc88bcd94bf2780f7c3f11095f4b529e80449";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    array base double-conversion ghc-prim integer-gmp old-locale text
    time transformers
  ];
  homepage = "https://github.com/bos/text-format";
  description = "Text formatting";
  license = stdenv.lib.licenses.bsd3;
}
