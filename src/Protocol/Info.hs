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
    } deriving (Eq, Show)

    data InfoDictionary = InfoDictionary{
        name:: !T.Text, -- 'name' -> suggested name to save the file
        pieceLength:: !Integer, -- 'piece length' -> lenght of each block
        pieces:: ![SHA1Hash], -- 
        fileInfos:: ![FileInfo]
    } deriving (Eq, Show)

    data FileInfo = FileInfo {
        length:: !Int, -- length of the file in bytes
        path:: ![T.Text] -- position, for a single file it's an empty list, meaning "this directory"
        } deriving (Eq, Show)

    newtype URL = URL T.Text deriving (Eq,Show)
    newtype SHA1Hash = SHA1Hash BS.ByteString deriving (Eq,Show)
    
    decodeMetaInfo:: BS.ByteString -> Either T.Text MetaInfo
    decodeMetaInfo bs  =  (first T.pack (decodeStrict bs)) >>= decodeTo

    extractFromDict :: Decoder b => BS.ByteString -> M.Map BS.ByteString BType -> Either T.Text  b
    extractFromDict key dict =
        (first E.decodeUtf8 (maybeToRight ("Could not find key "<> key) (M.lookup key dict))) >>= decodeTo

    class Decoder a where
        decodeTo :: BType -> Either T.Text a


    instance Decoder BS.ByteString where
        decodeTo (BString t) = pure t
        decodeTo other = Left (errorMsg "string" other)

    instance Decoder T.Text where
        decodeTo btype = E.decodeUtf8 <$> (decodeTo btype)

    instance Decoder Int64 where
        decodeTo (BInteger t) = pure t
        decodeTo other = Left (errorMsg "int64" other)

    instance Decoder InfoDictionary where
        decodeTo (BDict m) = do
            n <- extractFromDict "name" m
            p_length <- fmap toInteger (extractFromDict "piece length" m :: Either T.Text Int64)
            hash <- extractHash m
            infos <- extractInfos m
            return (InfoDictionary n p_length hash infos)
        decodeTo other = Left (errorMsg "dict" other)

    instance Decoder MetaInfo where
        decodeTo (BDict dict) = do
            announceUrl <- fmap URL (extractFromDict "announce" dict)
            infoDictionary <- extractFromDict "info"  dict
            return $ MetaInfo {announce = announceUrl, info = infoDictionary}
        decodeTo other = Left (errorMsg "dict" other)
    
        
    extractHash :: Dict -> Either T.Text [SHA1Hash]
    extractHash dict = do
        piecesStr <- extractFromDict "pieces" dict
        return (SHA1Hash <$> (chunksOf 20 piecesStr))

    extractInfos :: Dict -> Either T.Text [FileInfo]
    extractInfos dict= 
        case (extractFromDict "length" dict) of 
            (Right len) -> 
                Right [FileInfo (fromIntegral (len :: Int64)) [""]]
            Left _ -> 
                Left "Multiple files not supporteded yet."
    errorMsg :: T.Text -> BType -> T.Text
    errorMsg expected bType = "expected a " <> expected <> ", got: " <> T.pack (show bType)

    -- splits the pieces string into chunks of 20 chars
    chunksOf :: Int -> BS.ByteString -> [BS.ByteString]
    chunksOf i bs = 
        if (BS.length bs == 0) then
            []
        else 
            let (chunk, remaining) = BS.splitAt i bs
            in chunk : (chunksOf i remaining)