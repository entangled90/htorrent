{-# LANGUAGE OverloadedStrings #-}
module Protocol.BEncodeSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Protocol.BEncode
import           Data.Text                     as T
import           Data.Text.Encoding
import           Test.QuickCheck.Instances.Text ( )
import           Data.Map
import qualified Data.ByteString.Lazy as L

spec :: Spec
spec = describe "encode" $ do
    it "basic string check" $ encode (BString "spam") `shouldBe` "4:spam"
    it "basic int check" $ encode (BInteger 36) `shouldBe` "i36e"
    it "basic list check"
        $          encode (BList [BString "spam", BString "eggs"])
        `shouldBe` "l4:spam4:eggse"
    it "basic map check"
        $          encode
                       (BDict $ fromList
                           [("cow", BString "moo"), ("spam", BString "eggs")]
                       )
        `shouldBe` "d3:cow3:moo4:spam4:eggse"
    prop "string check" $ \s ->
        let encoded = (T.pack . show . T.length) s <> ":" <> s
        in  decodeUtf8 (L.toStrict $ encode (BString s)) `shouldBe` encoded
    -- it "overflow" $ plus2 maxBound `shouldBe` minBound + 1
    -- prop "minus 2" $ \i -> plus2 i - 2 `shouldBe` i
