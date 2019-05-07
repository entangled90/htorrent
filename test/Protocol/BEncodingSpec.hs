{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Protocol.BEncodingSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Protocol.BEncoding
import           Data.Text                     as T
import           Test.QuickCheck.Instances.Text ( )
import           Data.Map
import           Test.QuickCheck
import Debug.Trace
import Utils

spec :: Spec
spec = describe "BEncoding" $ do
    it "basic string check" $ encode (BString "spam") `shouldBe` "4:spam"
    it "empty string check" $ encode (BString "") `shouldBe` "0:"
    it "basic int check" $ encode (BInteger 36) `shouldBe` "i36e"
    
    it "basic list check" $
        encode (BList [BString "spam", BString "eggs"]) `shouldBe` "l4:spam4:eggse"
    it "basic dict check" $
        encode
            (BDict $ fromList[("cow", BString "moo"), ("spam", BString "eggs")])
            `shouldBe` "d3:cow3:moo4:spam4:eggse"
    it "basic empty dict check" $
        encode(BDict (fromList [])) `shouldBe` "de"
    
    it "initial torrent check" $ decodeText (
        "d8:announce39:http://torrent.ubuntu.com:6969/announce7:comment29:Kubuntu CD cdimage.ubuntu.com13:creation datei1555522851ee"
        ) `shouldSatisfy` isRight
    -- PROPS
    prop "string check" $ \s ->
        let encoded = (T.pack . show . T.length) s <> ":" <> s
        in  encode (BString s) `shouldBe` encoded
    prop "int encode-decode" $ \i -> case (decodeText . encode . BInteger) i of
        Right (BInteger j) -> j == i
        _                  -> False
    prop "str encode-decode" $ \s -> case (decodeText . encode . BString) s of
        Right (BString j) -> s == j
        _                 -> False
    prop "btype generic encode-decode" $ \s -> case (decodeText . encode) (btype s) of
        (Right j) ->
            -- traceShow j
            j == btype s
        other         ->
            trace "\n"
            traceShow other
            trace "\n"
            False

newtype Qcheck = Qcheck {btype :: BType} deriving Show

instance Arbitrary Qcheck where
    arbitrary =
        let primitiveGen =
                    oneof [fmap BString arbitrary, fmap BInteger arbitrary]
            btypeGen = oneof [primitiveGen, fmap BList (listOf primitiveGen), fmap (BDict . fromList) (listOf $ (,) <$> arbitrary <*> primitiveGen)]
        in  fmap Qcheck btypeGen

