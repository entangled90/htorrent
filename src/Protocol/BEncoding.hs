{-# LANGUAGE OverloadedStrings #-}

module Protocol.BEncoding (
    BType(..)
    , encode
    , encodeStrict
    , decodeStrict
    , dictionaryParser
    , Dict
    , BDecoder
    , decodeTo
    , extractFromDict
    , rawDictionary) where

    import Control.Applicative
 
    import qualified Data.Map as M

    import qualified Data.ByteString as BS
    import qualified Data.ByteString.Lazy as LBS

    import qualified Data.Text.Encoding as E
    import qualified Data.Text as T

    import Data.Either.Combinators

    import Data.ByteString.Builder
    import qualified Data.Attoparsec.ByteString as P
    import Data.Attoparsec.ByteString.Char8 as P8

    import Data.Bifunctor(first)

    import Data.Int

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
        BList ![BType] |
        BDict !(M.Map BS.ByteString BType)
        deriving (Eq, Show)

    type Dict = M.Map BS.ByteString BType

    encode:: BType -> LBS.ByteString
    encode  =
        let
            i = E.encodeUtf8Builder "i"
            e = E.encodeUtf8Builder "e"
            d = E.encodeUtf8Builder "d"
            l = E.encodeUtf8Builder "l"
            colon = E.encodeUtf8Builder ":"
            -- builders support fast appending, therefore we convert to bytestring only at the end.
            encodeBuilder:: BType -> Builder
            encodeBuilder (BString text) =  intDec (BS.length text) <> colon <> byteString text
            encodeBuilder (BInteger int) =  i <> int64Dec int <> e
            encodeBuilder (BList list) =
                let encodedElements = foldMap encodeBuilder list
                in l <> encodedElements <>  e
            encodeBuilder (BDict dictionary) =
                let encodeTuple (t, btype) = encodeBuilder (BString t) <> encodeBuilder btype
                    encodedEntries = foldMap encodeTuple (M.toAscList dictionary)
                in d <> encodedEntries <> e
        in toLazyByteString . encodeBuilder

    encodeStrict :: BType -> BS.ByteString
    encodeStrict = LBS.toStrict . encode

    decodeStrict:: BS.ByteString -> Either T.Text BType
    decodeStrict bs =
        let
            bTypeParser = intParser <|> strParser <|> listParser <|> dictParser

            intParser = BInteger <$> (P.string "i" *> P8.signed P8.decimal<* P.string "e")

            strParser = fmap BString textParser

            listParser = BList <$> (P.string "l" *> P.many' bTypeParser  <* P.string "e")

            dictParser = BDict <$> dictionaryParser bTypeParser
        -- let text = decodeUtf8 (LBS.toStrict bs)
        in toEither $ P.parse bTypeParser bs

    textParser :: P.Parser BS.ByteString
    textParser  = do
        len <- P8.decimal
        _ <- P.string ":"
        P.take (len :: Int)

    dictionaryParser :: P.Parser a -> P.Parser (M.Map BS.ByteString a )
    dictionaryParser innerParser =
        let parseTuple = (,) <$> textParser <*> innerParser
        in M.fromAscList <$> (P.string "d" *> P.many' parseTuple <* P.string "e")

    rawDictionary :: BS.ByteString -> Either T.Text (M.Map BS.ByteString BS.ByteString)
    rawDictionary bs = toEither $ P.parse (dictionaryParser mempty) bs

    toEither:: Show a => P.Result a -> Either T.Text a
    toEither (P.Done _ a) = Right a
    toEither failure  = Left $ "Parsing failed: " <> (T.pack $ show failure)

    extractFromDict :: BDecoder b => BS.ByteString -> M.Map BS.ByteString BType -> Either T.Text  b
    extractFromDict key dict =
        first E.decodeUtf8 (maybeToRight ("Could not find key "<> key) (M.lookup key dict)) >>= decodeTo

    class BDecoder a where
        decodeTo :: BType -> Either T.Text a

    instance BDecoder BS.ByteString where
        decodeTo (BString t) = pure t
        decodeTo other = Left (errorMsg "string" other)

    instance BDecoder T.Text where
        decodeTo btype = E.decodeUtf8 <$> decodeTo btype

    instance BDecoder Int64 where
        decodeTo (BInteger t) = pure t
        decodeTo other = Left (errorMsg "int64" other)

    errorMsg :: T.Text -> BType -> T.Text
    errorMsg expected bType = "expected a " <> expected <> ", got: " <> T.pack (show bType)