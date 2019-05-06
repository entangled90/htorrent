{-# LANGUAGE OverloadedStrings #-}

module Protocol.BEncode (BType, encode) where
import Data.Map
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Text.Encoding
import Data.Attoparsec

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
    BInteger !Int |
    BList [BType] |
    BDict !(Map T.Text BType)


encode:: BType -> BS.ByteString
encode (BString text) =
    let complete = encodeInt (T.length  text) <> ":" <> text
    in encodeUtf8 complete
encode (BInteger int) =
    encodeUtf8 ("i" <> encodeInt int <> "e")
encode (BList list) =
    let encodedElements = foldMap encode list
    in encodeUtf8 "l"  <> encodedElements <>  encodeUtf8 "e"
encode (BDict dictionary) =
    let encodeTuple (t, btype) = encode (BString t) <> encode btype
        encodedEntries = foldMap encodeTuple (toAscList dictionary)
    in encodeUtf8 "d" <> encodedEntries <> encodeUtf8 "e"


decode:: BS.ByteString -> Either String BType
decode bs = undefined

decodeString :: BS.ByteString -> Parser BType
decodeString bs = 
    let text = decodeUtf8 bs
    in undefined


encodeInt:: Int -> T.Text
encodeInt = T.pack . show