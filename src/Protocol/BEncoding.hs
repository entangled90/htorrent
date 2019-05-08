{-# LANGUAGE OverloadedStrings #-}

module Protocol.BEncoding (BType(..), encode, encodeStrict, decodeStrict) where


    import Control.Applicative

    import Data.Map
    import qualified Data.ByteString as BS
    import qualified Data.ByteString.Lazy as LBS

    import Data.Text.Encoding
    import Data.ByteString.Builder
    import Data.Int
    import qualified Data.Attoparsec.ByteString as P
    import Data.Attoparsec.ByteString.Char8

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
        BString !BS.ByteString |
        BInteger !Int64 |
        BList [BType] |
        BDict !(Map BS.ByteString BType)
        deriving (Eq, Show)


    encode:: BType -> LBS.ByteString
    encode  = toLazyByteString . encodeBuilder

    encodeStrict :: BType -> BS.ByteString
    encodeStrict = LBS.toStrict . encode

    -- builders support fast appending, therefore we convert to bytestring only at the end.
    encodeBuilder:: BType -> Builder
    encodeBuilder (BString text) =  intDec (BS.length text) <> colon <> byteString text
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

    decodeStrict:: BS.ByteString -> Either String BType
    decodeStrict bs =
        -- let text = decodeUtf8 (LBS.toStrict bs)
        toEither $ P.parse bTypeParser bs

    bTypeParser :: P.Parser BType
    bTypeParser = intParser <|> strParser <|> listParser <|> dictParser

    intParser :: P.Parser BType
    intParser = BInteger <$> (P.string "i" *> decimal <* P.string "e")

    strParser :: P.Parser BType
    strParser = fmap BString textParser

    listParser :: P.Parser BType
    listParser = BList <$> (P.string "l" *> many bTypeParser  <* P.string "e")

    dictParser :: P.Parser BType
    dictParser =
        let parseTuple = (,) <$> textParser <*> bTypeParser
        in BDict . fromAscList <$> (P.string "d" *> many parseTuple <* P.string "e")


    textParser :: P.Parser BS.ByteString
    textParser  = do
        len <- decimal
        _ <- P.string ":"
        P.take (len :: Int)

    toEither:: Show a => P.Result a -> Either String a
    toEither (P.Done _ a) = Right a
    toEither failure  = Left $ "Parsing failed" <> show failure
