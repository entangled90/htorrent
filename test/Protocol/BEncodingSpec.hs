{-# LANGUAGE OverloadedStrings #-}

module Protocol.BEncodingSpec where

import           Prelude
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Protocol.BEncoding
import           Test.QuickCheck.Instances.ByteString ( )
import           Data.Map
import           Test.QuickCheck
import Utils
import Protocol.Common

spec :: Spec
spec = describe "BEncoding" $ do
    contents <- runIO (fileContents)
    it "should encode a simple string" $ encode (BString "spam") `shouldBe` "4:spam"
    it "should encode an empty string" $ encode (BString "") `shouldBe` "0:"
    it "should encode an integer" $ encode (BInteger (-4)) `shouldBe` "i-4e"
    it "should decode an integer" $ decode "i-4e" `shouldBe` Right (BInteger (-4))
    it "should encode a simple list" $
        encode (BList [BString "spam", BString "eggs"]) `shouldBe` "l4:spam4:eggse"
    it "should encode a simple dict" $
        encode
            (BDict $ fromList[("cow", BString "moo"), ("spam", BString "eggs")])
            `shouldBe` "d3:cow3:moo4:spam4:eggse"
    it "should encode an empty dict" $
        encode(BDict (fromList [])) `shouldBe` "de"

    it "should decode a dict from a torrent" $ decode
        "d8:announce39:http://torrent.ubuntu.com:6969/announce7:comment29:Kubuntu CD cdimage.ubuntu.com13:creation datei1555522851e4:infod6:lengthi1916190720e4:name31:kubuntu-19.04-desktop-amd64.iso12:piece lengthi524288eee"
        `shouldSatisfy` isRight
    it "should decode torrent files from disk" $ 
        traverse decode contents `shouldSatisfy` isRight

    -- PROPS
    prop "int encode-decode" $ \i ->
        case (decode . encode . BInteger) i of
            Right (BInteger j) -> j == i
            _                  -> False
    prop "str encode-decode" $ \s -> case (decode . encode . BString) s of
        Right (BString j) -> s == j
        _                 -> False
    prop "btype generic encode-decode" $ \s -> case (decode . encode) (btype s) of
        (Right j) -> j == btype s
        _         -> False
    --  Let's load a file from disk.
    it "decode a torrent file" $ 
    -- (Right () :: Either () ())`shouldSatisfy` isRight
        traverse decode contents `shouldSatisfy` isRight

newtype Qcheck = Qcheck {btype :: BType} deriving Show

instance Arbitrary Qcheck where
    arbitrary =
        let primitiveGen =
                    oneof [fmap BString arbitrary, fmap BInteger arbitrary]
            btypeGen = oneof [primitiveGen, fmap BList (listOf primitiveGen), fmap (BDict . fromList) (listOf $ (,) <$> arbitrary <*> primitiveGen)]
        in  fmap Qcheck btypeGen

