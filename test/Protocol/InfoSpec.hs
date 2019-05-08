{-# LANGUAGE OverloadedStrings #-}

module Protocol.InfoSpec where

import Prelude
import           Test.Hspec
import           Protocol.Info
-- import           Test.QuickCheck.Instances.Text ( )
import qualified Data.ByteString as BS
import qualified Data.Text as T

spec :: Spec
spec = describe "encode" $ do
    contents <- runIO $ BS.readFile "./files/archlinux.torrent"
    it "decode a torrent file" $
    -- (Right () :: Either () ())`shouldSatisfy` isRight
        (name . info ) <$> (decodeMetaInfo contents)  `shouldBe` Right archName

archName :: T.Text 
archName = "archlinux-2019.05.02-x86_64.iso"
    -- PROPS
    -- prop "string check" $ \s ->
    --     let encoded = (T.pack . show . T.length) s <> ":" <> s
    --     in  decodeUtf8 (L.toStrict $ encode (BString s)) `shouldBe` encoded

-- liftEither :: Show a => Either a b -> IO b
-- liftEither (Right b) = pure b
-- liftEither (Left  a) = error $ show a
