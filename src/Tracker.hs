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
  import Data.ByteString as BS
  import           Network.HTTP.Simple
  import Data.ByteString.Conversion.To


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

  initialTrackerRequest:: HasConfig e => MetaInfo -> RIO e TrackerRequest
  initialTrackerRequest MetaInfo{..} = do
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

  getTrackerResponse :: MonadIO m => MetaInfo -> m TrackerResponse
  getTrackerResponse MetaInfo{..} req = 
    do
    request' <- parseRequest "GET " <> getUrl announce
    request = setRequestQueryString  ( <$> queryString req)
    

  data TrackerResponse = TrackerResponse {
    interval :: Int,
    peers :: [PeerId]
  }

  queryString :: TrackerRequest -> [(BS.ByteString, BS.ByteString)]
  queryString TrackerRequest{..} =
      let 
        eventTuple = case event of 
          Just evt -> [("event", toByteString evt)]
          Nothing -> []
      in
        [
          ("info_hash", toByteString infoHash)
          , ("peer_id", toByteString peerId)
          , ("ip", toByteString ip)
          , ("port", toByteString  port)
          , ("uploaded", toByteString  uploaded)
          , ("downloaded", toByteString downloaded)
          , ("left", toByteString bytesLeft)
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