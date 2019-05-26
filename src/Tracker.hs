{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Tracker where

  import RIO
  import qualified Data.Map as M
  import Protocol.Info
  import Protocol.BEncoding
  import Env
  import Data.Text as T

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

  fromMetaInfo:: HasConfig e => MetaInfo -> RIO e TrackerRequest
  fromMetaInfo MetaInfo{..} = do
    Config (Peer _id  _ip  _port) <- config <$> ask
    return TrackerRequest {
    infoHash = infoHash
    , peerId = _id
    , ip = _ip
    , port = _port
    , uploaded = 0
    , downloaded = 0
    , bytesLeft = pieceLength info
    , event = Nothing
  }


  data TrackerResponse = TrackerResponse {
    interval :: Int,
    peers :: [PeerId]
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
  
  instance BDecoder TrackerResponse where 
    decodeTo (BDict m) = do 
      case M.lookup "failure_reason" m of 
        Just err -> Left $ "Tracker responded with failure: " <> (T.pack $ show err)
        Nothing -> return () 
      _interval <- extractFromDict "interval" m 
      _peers <- extractFromDict "peers" m
      return (TrackerResponse _interval _peers)
    decodeTo unexpected = Left $ errorMsg "dictionary" unexpected