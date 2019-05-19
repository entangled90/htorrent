{-# LANGUAGE OverloadedStrings #-}

module TrackerSpec where

import Tracker
import           Prelude
import           Test.Hspec
import Protocol.Info(mkSHA1Hash)
import Protocol.BEncoding
import qualified Data.ByteString as BS

import Debug.Trace

req :: TrackerRequest
req = TrackerRequest{
  infoHash = (mkSHA1Hash "ciao scemo")
  , peerId = PeerId ("ciao ciao ciao ciao ")
  , ip = "192.168.1.1"
  , port = 6981
  , uploaded = 923
  , downloaded = 9091023
  , left = 199123
  , event = Nothing
}





spec :: Spec
spec = describe "Tracker" $ do
    it "should encode tracker requests correctly" $ 
      let encoded = (encode . encodeFrom) req
      in traceShow encoded encoded `shouldSatisfy` (\bs -> BS.length bs > 0)

