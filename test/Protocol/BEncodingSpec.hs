{-# LANGUAGE OverloadedStrings #-}

module Protocol.BEncodingSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Protocol.BEncoding
import           Test.QuickCheck.Instances.ByteString ( )
import           Data.Map
import           Test.QuickCheck
import Debug.Trace
import Utils

spec :: Spec
spec = describe "BEncoding" $ do
    it "should encode a simple string" $ encode (BString "spam") `shouldBe` "4:spam"
    it "should encode an empty string" $ encode (BString "") `shouldBe` "0:"
    it "should encode an integer" $ encode (BInteger 36) `shouldBe` "i36e"

    it "should encode a simple list" $
        encode (BList [BString "spam", BString "eggs"]) `shouldBe` "l4:spam4:eggse"
    it "should encode a simple dict" $
        encode
            (BDict $ fromList[("cow", BString "moo"), ("spam", BString "eggs")])
            `shouldBe` "d3:cow3:moo4:spam4:eggse"
    it "should encode an empty dict" $
        encode(BDict (fromList [])) `shouldBe` "de"

    it "should decode a dict from a torrent torrent" $ decodeStrict
        "d8:announce39:http://torrent.ubuntu.com:6969/announce7:comment29:Kubuntu CD cdimage.ubuntu.com13:creation datei1555522851e4:infod6:lengthi1916190720e4:name31:kubuntu-19.04-desktop-amd64.iso12:piece lengthi524288eee" 
        `shouldSatisfy` isRight
    
    -- PROPS
    prop "int encode-decode" $ \i -> case (decodeStrict . encodeStrict . BInteger) i of
        Right (BInteger j) -> j == i
        _                  -> False
    prop "str encode-decode" $ \s -> case (decodeStrict . encodeStrict . BString) s of
        Right (BString j) -> s == j
        _                 -> False
    prop "btype generic encode-decode" $ \s -> case (decodeStrict . encodeStrict) (btype s) of
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

