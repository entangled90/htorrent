-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Protocol.Info where

    import Data.Text
    import Data.Either.Combinators
    import qualified Data.Map.Strict as M
    import Protocol.BEncoding

    data MetaInfo = MetaInfo {
        announce:: URL,
        info:: InfoDictionary
    }

    data InfoDictionary = InfoDictionary{
        name:: !Text, -- suggested name to save the file
        pieceLength:: !Integer,
        pieces:: ![SHA1Hash],
        fileInfos:: ![FileInfo]
    }

    data FileInfo = FileInfo {
        length:: !Int, -- length of the file in bytes
        path:: ![Text] -- position, for a single file it's an empty list, meaning "this directory"
        }

    newtype URL = URL Text deriving (Eq,Show)
    newtype SHA1Hash = SHA1Hash Text deriving (Eq,Show)
    decodeMetaInfo:: Text -> Either Text MetaInfo
    decodeMetaInfo txt = decodeText txt >>= fromBType

    fromBType :: BType -> Either Text MetaInfo
    fromBType (BDict map) = do
        announceUrl <- extractFromDict "announce" (fmap URL . extractString ) (BDict map)
        info <- maybeToRight "missing info" (M.lookup "info" map)
        return $ MetaInfo {announce = announceUrl, info = undefined}
    fromBType other = Left $ pack $ "expected a dictionary, got: " <> show other

    extractFromDict :: Text -> (BType -> Maybe b) -> BType ->  Either Text  b
    extractFromDict key mapper (BDict map) =
        maybeToRight ("Could not find key "<> key) (M.lookup key map >>= mapper )
    extractFromDict _ _ _ = Left "Btype is not a map"

    extractString :: BType -> Maybe Text
    extractString (BString t) = pure t
    extractString _ = Nothing