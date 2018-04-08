{ mkDerivation, base, bytestring, containers, directory, filepath
, pipes, pipes-bytestring, protolude, python-pickle, stdenv
, transformers, zlib
}:
mkDerivation {
  pname = "unrpa";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers directory filepath pipes
    pipes-bytestring protolude python-pickle transformers zlib
  ];
  homepage = "https://github.com/pbogdan/unrpa";
  description = "Extractor for RPA archives used by RenPy visual novel engine";
  license = stdenv.lib.licenses.bsd3;
}
