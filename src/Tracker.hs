{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Tracker where

  import qualified Data.Map as M
  import Protocol.Info
  import Protocol.BEncoding
  import Data.Text as T
  import Data.IP

  data TrackerRequest = TrackerRequest{
      infoHash :: !SHA1Hash
    , peerId :: !PeerId
    , ip :: !IpAddr -- our ip 
    , port :: !Port -- our port 
    , uploaded :: !Int -- bytes uploaded
    , downloaded :: !Int -- bytes downloaded
    , bytesLeft :: !Int -- Bytes left
    , event :: !(Maybe TrackerEvent)
  }


  data TrackerResponse = TrackerResponse {
    interval :: Int,
    peers :: [PeerId]
  }
  
  data Peer = Peer{
    peerId:: PeerId,
    ip :: IpAddr,
    port :: Port
  }

  queryString :: TrackerRequest -> [(String, String)]
  queryString TrackerRequest{..} =
      let 
        eventTuple = case event of 
          Just evt -> [("event", show evt)]
          Nothing -> []
      in
        [
          ("info_hash", show infoHash)
          , ("peer_id", show peerId)
          , ("ip", show ip)
          , ("port", show  port)
          , ("uploaded", show  uploaded)
          , ("downloaded", show downloaded)
          , ("left", show bytesLeft)
        ] ++ eventTuple


  data TrackerEvent = Started | Completed | Stopped

  instance Show TrackerEvent where
    show Started = "started"
    show Completed = "completed"
    show Stopped = "stopped"
  
  newtype PeerId = PeerId {id :: T.Text} deriving (Eq, Show, BEncoder, BDecoder)

  newtype Port = Port {port:: Int} deriving (Eq, Show, BEncoder, BDecoder)

  newtype IpAddr = IpAddr {ipAddr :: IP} deriving (Eq, Show)


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

  instance BDecoder TrackerResponse where 
    decodeTo (BDict m) = do 
      case M.lookup "failure_reason" m of 
        Just err -> Left $ "Tracker responded with failure: " <> (T.pack $ show err)
        Nothing -> return () 
      _interval <- extractFromDict "interval" m 
      _peers <- extractFromDict "peers" m
      return (TrackerResponse _interval _peers)
    decodeTo unexpected = Left $ errorMsg "dictionary" unexpected

  instance BDecoder IpAddr where
    decodeTo (BString _ipAddr) = (pure . IpAddr .  read . show) _ipAddr
    decodeTo err = Left $ errorMsg "ip addr" err