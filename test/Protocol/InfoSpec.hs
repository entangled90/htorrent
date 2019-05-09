{-# LANGUAGE OverloadedStrings #-}

module Protocol.InfoSpec where

import Prelude
import           Test.Hspec
import           Protocol.Info
import qualified Data.ByteString as BS
import qualified Data.Text as T()

spec :: Spec
spec = describe "encode" $ do
    contentsList <- runIO ( traverse BS.readFile files)
    it "decode a torrent file" $
        let 
            check (c, m) = 
                case decodeMetaInfo c of
                    Right(actual) -> verifyMetaInfo m actual
                    Left _ -> expectationFailure "decoding failed"
        in foldMap check (zip contentsList expected)


files :: [String]
files =["files/archlinux.torrent", "files/kubuntu.torrent"]

expected :: [MetaInfo]
expected = [
    MetaInfo 
        (URL "http://tracker.archlinux.org:6969/announce") 
        (InfoDictionary "archlinux-2019.05.02-x86_64.iso" 524288 [] [
            FileInfo 638582784 [""]
        ])  ,
    MetaInfo 
        (URL "http://torrent.ubuntu.com:6969/announce") 
        (InfoDictionary "kubuntu-19.04-desktop-amd64.iso" 524288 [] [
            FileInfo 1916190720 [""]
        ])]
verifyMetaInfo:: MetaInfo -> MetaInfo -> Expectation
verifyMetaInfo e actual = do
    announce e `shouldBe` announce actual
    let 
        expInfo = info e
        actInfo = info actual
    name expInfo `shouldBe` name actInfo
    pieceLength expInfo `shouldBe` pieceLength actInfo
    length (fileInfos actInfo) `shouldBe` 1

    -- PROPS
    -- prop "string check" $ \s ->
    --     let encoded = (T.pack . show . T.length) s <> ":" <> s
    --     in  decodeUtf8 (L.toStrict $ encode (BString s)) `shouldBe` encoded

-- liftEither :: Show a => Either a b -> IO b
-- liftEither (Right b) = pure b
-- liftEither (Left  a) = error $ show a
