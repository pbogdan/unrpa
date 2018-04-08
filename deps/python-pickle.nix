{ mkDerivation, attoparsec, attoparsec-binary, base, bytestring
, cereal, cmdargs, containers, directory, fetchgit, HUnit, mtl
, process, QuickCheck, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2, testing-feat, text
}:
mkDerivation {
  pname = "python-pickle";
  version = "0.2.3";
  src = fetchgit {
    url = "https://github.com/lumimies/python-pickle.git";
    sha256 = "1yx21sxjdihxvn65iam0gkl8cksm513rdcgr6jdy4b3fvrma2k1r";
    rev = "ec3ad7e18964a9153227945ab3963bced5742f80";
  };
  isLibrary = true;
  patches = [ ./patch.patch ];
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec attoparsec-binary base bytestring cereal containers mtl
    text
  ];
  executableHaskellDepends = [ base bytestring cmdargs ];
  testHaskellDepends = [
    base bytestring cereal containers directory HUnit process
    QuickCheck test-framework test-framework-hunit
    test-framework-quickcheck2 testing-feat text
  ];
  jailbreak = true;
  doCheck = false;
  description = "Serialization/deserialization using Python Pickle format";
  license = stdenv.lib.licenses.bsd3;
}
