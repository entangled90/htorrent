{-# LANGUAGE OverloadedStrings #-}

module Protocol.Info where

    import Prelude
    import qualified Data.Text as T
    import qualified Data.Text.Encoding as E
    import qualified Data.ByteString as BS
    import Data.Either.Combinators
    import qualified Data.Map as M
    import Protocol.BEncoding
    import Data.Int
    import Data.Bifunctor(first)

    data MetaInfo = MetaInfo {
        announce:: !URL,
        info:: InfoDictionary
    }

    data InfoDictionary = InfoDictionary{
        name:: !T.Text, -- suggested name to save the file
        pieceLength:: !Integer,
        pieces:: ![SHA1Hash],
        fileInfos:: ![FileInfo]
    }

    data FileInfo = FileInfo {
        length:: !Int, -- length of the file in bytes
        path:: ![T.Text] -- position, for a single file it's an empty list, meaning "this directory"
        }

    newtype URL = URL T.Text deriving (Eq,Show)
    newtype SHA1Hash = SHA1Hash T.Text deriving (Eq,Show)
    
    decodeMetaInfo:: BS.ByteString -> Either T.Text MetaInfo
    decodeMetaInfo bs  =  (first T.pack (decodeStrict bs)) >>= decodeTo

    extractFromDict :: Decoder b => BS.ByteString -> M.Map BS.ByteString BType -> Either T.Text  b
    extractFromDict key dict =
        (first E.decodeUtf8 (maybeToRight ("Could not find key "<> key) (M.lookup key dict))) >>= decodeTo

    class Decoder a where
        decodeTo :: BType -> Either T.Text a


    instance Decoder T.Text where
        decodeTo (BString t) = pure $ E.decodeUtf8 t
        decodeTo other = Left ("Invalid field, expected string got " <> T.pack (show other))

    instance Decoder Int64 where
        decodeTo (BInteger t) = pure t
        decodeTo other = Left ("Invalid field, expected integer got " <> T.pack (show other))

    instance Decoder InfoDictionary where
        decodeTo (BDict m) = do
            n <- extractFromDict "name" m
            p_length <- fmap toInteger (extractFromDict "piece length" m :: Either T.Text Int64)
            return (InfoDictionary n p_length [] [])
        decodeTo other = Left ("Invalid field, expected dictionary got " <> T.pack (show other))

    instance Decoder MetaInfo where
        decodeTo (BDict dict) = do
            announceUrl <- fmap URL (extractFromDict "announce" dict)
            infoDictionary <- extractFromDict "info"  dict
            return $ MetaInfo {announce = announceUrl, info = infoDictionary}
        decodeTo other = Left $ T.pack $ "expected a dictionary, got: " <> show other