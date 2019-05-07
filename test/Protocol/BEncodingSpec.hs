{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Protocol.BEncodingSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Protocol.BEncoding               ( BType(..)
                                                , encode
                                                , decode
                                                )
import           Data.Text                     as T
import           Data.Text.Encoding
import           Test.QuickCheck.Instances.Text ( )
import           Data.Map
import qualified Data.ByteString.Lazy          as L
import           Test.QuickCheck
import Debug.Trace

spec :: Spec
spec = describe "encode" $ do
    it "basic string check" $ encode (BString "spam") `shouldBe` "4:spam"
    it "basic int check" $ encode (BInteger 36) `shouldBe` "i36e"
    it "basic list check" $
        encode (BList [BString "spam", BString "eggs"]) `shouldBe` "l4:spam4:eggse"
    it "basic dict check" $
        encode
            (BDict $ fromList[("cow", BString "moo"), ("spam", BString "eggs")])
            `shouldBe` "d3:cow3:moo4:spam4:eggse"
    it "basic empty dict check" $
        encode(BDict (fromList [])) `shouldBe` "de"

    -- PROPS
    prop "string check" $ \s ->
        let encoded = (T.pack . show . T.length) s <> ":" <> s
        in  decodeUtf8 (L.toStrict $ encode (BString s)) `shouldBe` encoded
    prop "int encode-decode" $ \i -> case (decode . encode . BInteger) i of
        Right (BInteger j) -> j == i
        _                  -> False
    prop "str encode-decode" $ \s -> case (decode . encode . BString) s of
        Right (BString j) -> s == j
        _                 -> False
    prop "btype generic encode-decode" $ \s -> case (decode . encode) (btype s) of
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

