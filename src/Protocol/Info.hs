{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Protocol.Info where

    import qualified Data.Map as M
    import qualified Data.Text as T
    import qualified Data.ByteString as BS
    -- import Data.Either.Combinators
    import Data.Int
    import Crypto.Hash.SHA1
    import Protocol.BEncoding

    data MetaInfo = MetaInfo {
        announce:: !URL,
        info :: !InfoDictionary,
        infoHash:: !SHA1Hash
    } deriving (Eq, Show)

    data InfoDictionary = InfoDictionary{
        name:: !T.Text, -- 'name' -> suggested name to save the file
        pieceLength:: !Int, -- 'piece length' -> lenght of each block
        pieces:: ![SHA1Hash],
        fileInfos:: ![FileInfo]
    } deriving (Eq, Show)

    data FileInfo = FileInfo {
        fileLength:: !Int, -- length of the file in bytes
        path:: ![T.Text] -- position, for a single file it's an empty list, meaning "this directory"
        } deriving (Eq, Show)

    newtype URL = URL {getUrl :: T.Text} deriving (Eq, Show, BEncoder)

    newtype SHA1Hash = SHA1Hash {sha1Hash:: BS.ByteString} deriving (Eq,Show, BEncoder)

    mkSHA1Hash:: BS.ByteString -> SHA1Hash
    mkSHA1Hash =  SHA1Hash . hash

    decodeMetaInfo::
        BS.ByteString -> -- ^ complete bytestring
        Either T.Text MetaInfo
    decodeMetaInfo bs =
        do
            decoded <- decode bs
            let fromDict (BDict dict) = do
                    announceUrl <- fmap URL (extractFromDict "announce" dict)
                    infoDictionary <- extractFromDict "info"  dict
                    let infoSHA1 =  hashInfoSection infoDictionary 
                    return $ MetaInfo {announce = announceUrl, info = infoDictionary, infoHash = infoSHA1}
                fromDict other = Left ("expected dict, found: " <> T.pack (show other))
            fromDict decoded

    instance BDecoder InfoDictionary where
        decodeTo (BDict m) = do
            n <- extractFromDict "name" m
            p_length <- fmap fromIntegral (extractFromDict "piece length" m :: Either T.Text Int64)
            piecesHash <- extractPiecesHash m
            infos <- extractInfos m
            return (InfoDictionary n p_length piecesHash infos)
        decodeTo other = Left $ "expected dict, found: " <> (T.pack $ show other)

    extractPiecesHash :: Dict -> Either T.Text [SHA1Hash]
    extractPiecesHash dict = do
        piecesStr <- extractFromDict "pieces" dict
        return (SHA1Hash <$> chunksOf 20 piecesStr)

    extractInfos :: Dict -> Either T.Text [FileInfo]
    extractInfos dict=
        case extractFromDict "length" dict of
            (Right len) ->
                Right [FileInfo (fromIntegral (len :: Int64)) [""]]
            Left _ ->
                Left "Multiple files not supporteded yet."

    hashInfoSection:: InfoDictionary -> SHA1Hash
    hashInfoSection  = mkSHA1Hash . encode . encodeFrom


    instance BEncoder InfoDictionary where
        encodeFrom infoDict = 
            BDict(
                M.fromAscList [
                    ("name", encodeFrom $ name infoDict)
                    , ("piece length", encodeFrom $ pieceLength infoDict)
                    , ("pieces", (encodeFrom . (foldMap sha1Hash) . pieces ) infoDict)
                    , ("length", (encodeFrom . fileLength . head . fileInfos) infoDict)
                ]
            )

    -- splits the pieces string into chunks of 20 chars
    chunksOf :: Int -> BS.ByteString -> [BS.ByteString]
chunksOf i (PS bs1 bs2 bs3) =
                      if BS.length bs == 0 then
                          []
                      else
                          let (chunk, remaining) = BS.splitAt i bs
                          in chunk : chunksOf i remaining