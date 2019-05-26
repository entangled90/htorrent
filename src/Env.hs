{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Env where
  import qualified Data.Text as T
  import Protocol.BEncoding
  import Data.IP

  data Env = Env {
    conf :: Config
  }

  data Config = Config {
    self:: Peer
  }

  class HasConfig a where
    config :: a -> Config

  instance HasConfig Env where
    config Env{..} = conf
  
  instance HasConfig Config where
    config c = c


  newtype PeerId = PeerId {getId :: T.Text} deriving (Eq, Show, BEncoder, BDecoder)

  newtype Port = Port {getPort:: Int} deriving (Eq, Show, BEncoder, BDecoder)

  newtype IpAddr = IpAddr {getIpAddr :: IP} deriving (Eq, Show)

  data Peer = Peer{
    id:: PeerId,
    ip :: IpAddr,
    port :: Port
  }

  mkPeerId :: T.Text -> Either T.Text PeerId
  mkPeerId str = 
    if T.length str == 20 then pure $ PeerId str 
    else Left $ "Invalid length: expected 20, found " <> (T.pack . show . T.length) str
  

  instance BDecoder Peer where
    decodeTo (BDict m) = do
      _peerId <- extractFromDict "peer id" m
      _ip <- extractFromDict "ip" m 
      _port <- extractFromDict "port" m
      return (Peer _peerId _ip _port)
    decodeTo unexpected = Left $ errorMsg "dictionary" unexpected
  
  instance BDecoder IpAddr where
    decodeTo (BString _ipAddr) = (pure . IpAddr .  read . show) _ipAddr
    decodeTo err = Left $ errorMsg "ip addr" err