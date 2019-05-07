{-# LANGUAGE OverloadedStrings #-}

module Protocol.BEncoding (BType(..), encode, decode) where


    import Control.Applicative

    import Data.Map
    import qualified Data.Text as T
    import qualified Data.ByteString.Lazy as L
    import Data.Text.Encoding
    import Data.ByteString.Builder
    import Data.Int
    import qualified Data.Attoparsec.Text as P


    {- BEncoding

        # Strings:
            Strings are length-prefixed base ten followed by a colon and the string. For example 4:spam corresponds to 'spam'.
        # Integers:
        Integers are represented by an 'i' followed by the number in base 10 followed by an 'e'. 
            For example i3e corresponds to 3 and i-3e corresponds to -3. Integers have no size limitation. i-0e is invalid. 
            All encodings with a leading zero, such as i03e, are invalid, other than i0e, which of course corresponds to 0.
        # Lists:
            Lists are encoded as an 'l' followed by their elements (also bencoded) followed by an 'e'. 
            For example l4:spam4:eggse corresponds to ['spam', 'eggs'].
        # Dictionaries:
            Dictionaries are encoded as a 'd' followed by a list of alternating keys and their corresponding values followed by an 'e'. 
            For example, d3:cow3:moo4:spam4:eggse corresponds to {'cow': 'moo', 'spam': 'eggs'} and d4:spaml1:a1:bee 
            corresponds to {'spam': ['a', 'b']}. Keys must be strings and appear in sorted order (sorted as raw strings, not alphanumerics).
    -}


    data BType =
        BString !T.Text |
        BInteger !Int64 |
        BList [BType] |
        BDict !(Map T.Text BType)
        deriving (Eq, Show)


    encode:: BType -> L.ByteString
    encode b = toLazyByteString $ encodeBuilder b

    -- builders support fast appending, therefore we convert to bytestring only at the end.
    encodeBuilder:: BType -> Builder
    encodeBuilder (BString text) =  intDec (T.length  text) <> colon <> encodeUtf8Builder text
    encodeBuilder (BInteger int) =  i <> int64Dec int <> e
    encodeBuilder (BList list) =
        let encodedElements = foldMap encodeBuilder list
        in l <> encodedElements <>  e
    encodeBuilder (BDict dictionary) =
        let encodeTuple (t, btype) = encodeBuilder (BString t) <> encodeBuilder btype
            encodedEntries = foldMap encodeTuple (toAscList dictionary)
        in d <> encodedEntries <> e

    -- Internal common constants
    i :: Builder
    i = encodeUtf8Builder "i"
    e :: Builder
    e = encodeUtf8Builder "e"
    d :: Builder
    d = encodeUtf8Builder "d"
    l :: Builder
    l = encodeUtf8Builder "l"

    colon :: Builder
    colon = encodeUtf8Builder ":"

    decode:: L.ByteString -> Either String BType
    decode bs =
        let text = decodeUtf8 (L.toStrict bs)
        in toEither $ P.parse bTypeParser text

    bTypeParser :: P.Parser BType
    bTypeParser = intParser <|> strParser <|> listParser <|> dictParser

    intParser :: P.Parser BType
    intParser = BInteger <$> (P.string "i" *> P.signed P.decimal <* P.string "e")

    strParser :: P.Parser BType
    strParser = fmap BString textParser

    listParser :: P.Parser BType
    listParser = BList <$> (P.string "l" *> many bTypeParser  <* P.string "e")

    dictParser :: P.Parser BType
    dictParser = 
        let parseTuple = (,) <$> textParser <*> bTypeParser
        in BDict . fromAscList <$> (P.string "d" *> many parseTuple <* P.string "e")


    textParser :: P.Parser T.Text
    textParser  = do 
        len <- P.decimal
        _ <- P.string ":"
        P.take len

    toEither:: Show a => P.Result a -> Either String a
    toEither (P.Done _ a) = Right a
    toEither failure  = Left $ "Parsing failed" <> show failure
