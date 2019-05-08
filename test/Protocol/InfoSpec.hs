{-# LANGUAGE OverloadedStrings #-}

module Protocol.InfoSpec where

import           Test.Hspec
import           Protocol.BEncoding
import           Protocol.Info                  ( )
import           Data.Text.IO                  as TIO
import           Test.QuickCheck.Instances.Text ( )
import Debug.Trace
import Utils
spec :: Spec
spec = describe "encode" $ do
    contents <- runIO $ TIO.readFile "./files/ubuntu.torrent"
    it "decode a torrent file" $
        let decodeResult = decodeText contents
        in traceShow decodeResult decodeResult `shouldSatisfy` isRight

    -- PROPS
    -- prop "string check" $ \s ->
    --     let encoded = (T.pack . show . T.length) s <> ":" <> s
    --     in  decodeUtf8 (L.toStrict $ encode (BString s)) `shouldBe` encoded

-- liftEither :: Show a => Either a b -> IO b
-- liftEither (Right b) = pure b
-- liftEither (Left  a) = error $ show a
