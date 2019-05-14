{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Protocol.Info where

    import qualified Data.Map as M
    import qualified Data.Text as T
    import qualified Data.ByteString as BS
    import Data.Either.Combinators
    import Data.Int
    import Crypto.Hash.SHA1
    import Debug.Trace
    import Protocol.BEncoding

    data MetaInfo = MetaInfo {
        announce:: !URL,
        info:: !InfoDictionary,
        infoHash:: !SHA1Hash
    } deriving (Eq, Show)

    data InfoDictionary = InfoDictionary{
        name:: !T.Text, -- 'name' -> suggested name to save the file
        pieceLength:: !Integer, -- 'piece length' -> lenght of each block
        pieces:: ![SHA1Hash],
        fileInfos:: ![FileInfo]
    } deriving (Eq, Show)

    data FileInfo = FileInfo {
        fileLength:: !Int, -- length of the file in bytes
        path:: ![T.Text] -- position, for a single file it's an empty list, meaning "this directory"
        } deriving (Eq, Show)

    newtype URL = URL T.Text deriving (Eq,Show)

    newtype SHA1Hash = SHA1Hash BS.ByteString deriving (Eq,Show)

    mkSHA1Hash:: BS.ByteString -> SHA1Hash
    mkSHA1Hash =  SHA1Hash . hash

    decodeMetaInfo::
        BS.ByteString -> -- ^ complete bytestring
        Either T.Text MetaInfo
    decodeMetaInfo bs =
        do
            infoSHA1 <- hashInfoSection bs
            let fromDict (BDict dict) = do
                    announceUrl <- fmap URL (extractFromDict "announce" dict)
                    infoDictionary <- extractFromDict "info"  dict
                    return $ MetaInfo {announce = announceUrl, info = infoDictionary, infoHash = infoSHA1}
                fromDict  other = Left ("expected dict, found: " <> T.pack (show other))
            decoded <- decodeStrict bs
            fromDict decoded

    instance BDecoder InfoDictionary where
        decodeTo (BDict m) = do
            n <- extractFromDict "name" m
            p_length <- fmap toInteger (extractFromDict "piece length" m :: Either T.Text Int64)
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

    hashInfoSection:: BS.ByteString -> Either T.Text SHA1Hash
    hashInfoSection bs =  do
        rawDict <- rawDictionary bs
        traceM (show rawDict)
        infoBs <-maybeToRight "Could not find key [info]" (M.lookup "info" rawDict)
        return $ mkSHA1Hash infoBs


    -- splits the pieces string into chunks of 20 chars
    chunksOf :: Int -> BS.ByteString -> [BS.ByteString]
    chunksOf i bs =
        if BS.length bs == 0 then
            []
        else
            let (chunk, remaining) = BS.splitAt i bs
            in chunk : chunksOf i remaining