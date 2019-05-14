module Protocol.Common where

import qualified Data.ByteString as BS

files :: [String]
files = ["files/archlinux.torrent", "files/kubuntu.torrent"]

fileContents :: IO [BS.ByteString]
fileContents =  traverse BS.readFile files