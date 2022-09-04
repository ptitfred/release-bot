{ mkDerivation, aeson, base, containers, formatting, hpack
, http-api-data, http-client-tls, lib, mtl, servant, servant-client
, servant-server, text, wai-extra, warp
}:
mkDerivation {
  pname = "release-bot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers formatting http-api-data http-client-tls mtl
    servant servant-client servant-server text wai-extra warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base containers formatting http-api-data http-client-tls mtl
    servant servant-client servant-server text wai-extra warp
  ];
  testHaskellDepends = [
    aeson base containers formatting http-api-data http-client-tls mtl
    servant servant-client servant-server text wai-extra warp
  ];
  prePatch = "hpack";
  homepage = "https://github.com/ptitfred/release-bot#readme";
  license = lib.licenses.bsd3;
}
