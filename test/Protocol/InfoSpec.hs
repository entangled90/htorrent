{-# LANGUAGE OverloadedStrings #-}

module Protocol.InfoSpec where

import           Prelude
import           Test.Hspec
import           Protocol.Info
import Protocol.Common

spec :: Spec
spec = describe "encode" $ do
    contents <- runIO fileContents
    it "decode a torrent file"
        $ let check (c, m) = case decodeMetaInfo c of
                  Right actual -> verifyMetaInfo m actual
                  Left err ->
                      expectationFailure ("decoding failed" <> (take 100 . show ) err)
          in  foldMap check (zip contents expected)


expected :: [MetaInfo]
expected =
    [ MetaInfo
        (URL "http://tracker.archlinux.org:6969/announce")
        (InfoDictionary "archlinux-2019.05.02-x86_64.iso"
                        524288
                        []
                        [FileInfo 638582784 [""]]
        )
        (SHA1Hash "")
    , MetaInfo
        (URL "http://torrent.ubuntu.com:6969/announce")
        (InfoDictionary "kubuntu-19.04-desktop-amd64.iso"
                        524288
                        []
                        [FileInfo 1916190720 [""]]
        )
        (SHA1Hash "")
    ]
verifyMetaInfo :: MetaInfo -> MetaInfo -> Expectation
verifyMetaInfo e actual = do
    announce e `shouldBe` announce actual
    let expInfo = info e
        actInfo = info actual
    name expInfo `shouldBe` name actInfo
    pieceLength expInfo `shouldBe` pieceLength actInfo
    length (fileInfos actInfo) `shouldBe` 1

