{ mkDerivation, base, bytestring, errors, http-client, http-client-tls, HUnit, lrucaching
, optparse-applicative, c-phash, hs-phash, pkgconfig, postgresql-simple, resourcet, stdenv
, stm, streaming, streaming-concurrency
, streaming-postgresql-simple, text, zeromq
}:
mkDerivation {
  pname = "facebook-ad-image-hashes";
  version = "0.1.0.0";
  src = ./.;
  buildTools = [ pkgconfig ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring errors HUnit lrucaching optparse-applicative hs-phash http-client
    http-client-tls postgresql-simple resourcet stm streaming streaming-concurrency
    streaming-postgresql-simple text
  ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
  libraryPkgconfigDepends = [ c-phash zeromq ];
  testPkgconfigDepends = [ c-phash zeromq ];
  executablePkgconfigDepends = [ c-phash zeromq ];
}
