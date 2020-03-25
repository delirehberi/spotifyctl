{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module SpotifyCommand where

import Text.RawString.QQ
import SpotifyApi
import Network.Wai
import qualified Network.Wai as W
import Network.HTTP.Types (status200,status404)
import Network.Wai.Handler.Warp (run)
import System.Exit
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Wreq as WR
import Data.Maybe (fromJust)
import           Control.Monad                  ( when )
import           Control.Concurrent.MVar
import           Control.Concurrent.Async

import Control.Lens
import Data.Aeson.Lens (_String, key, nth)
import Data.Aeson 
help :: IO ()
help = putStr [r|
  -h                Help 

  auth              Authorize the spotify client via OAuth
  next              Play next song 
  prev              Play previous song 
  pause             Pause the currently playing song
  play              Play the last paused song 
  play SONGID       Play the song which defined in given id
  search SONGNAME   Search for a song and returns the list of ids
  current           Returns currently playing song
  device -h         Returns device subcommand help text
  |]


auth :: IO ()
auth = do
  spkey <- spotifyKey
  putStrLn "Login URL:"
  putStrLn $ generateAuthUrl spkey 
  putStrLn "Waiting for logging in..." 
  toDie <- newEmptyMVar
  race_ (takeMVar toDie) $ run 9988 $ authApp toDie

authApp :: MVar () -> Request ->
  (Response -> IO ResponseReceived) -> 
  IO ResponseReceived
authApp toDie req resp = 
  case pathInfo req of
      ["oauthCallback"] -> do 
        spkey <- spotifyKey
        (r,code) <- authHandler req
        t <- fetchToken spkey code
        saveToken (t ^. WR.responseBody)
        putStrLn "Authorization completed. You can use the other commands now."
        putMVar toDie ()
        resp r
      x -> resp $ responseLBS status404 [("Content-Type","text/html")] "Content not found"
  

authHandler :: Request -> IO (Response,BSL.ByteString)
authHandler req = do 
    let code = fromJust $ lookup "code" (W.queryString req)
    case code of 
      Just x ->  return (responseLBS status200 [("Content-Type","text/plain")] $ "You can close the window now.", BSL.fromStrict x)
      Nothing -> return (responseLBS status200 [("Content-Type","text/plain")] "Code not exists in url", "")
      
refresh :: IO ()
refresh = do
  token <- readToken
  t <- refreshToken token    
  putStrLn "Ok"

nextSong :: IO () 
nextSong = do 
  token <- readToken
  result <- touchRequest token "next" "POST"
  case result of
    True -> return ()
    False -> error "Command wont work."

previousSong :: IO ()
previousSong  = do
  token <- readToken
  result <- touchRequest token "previous" "POST"
  case result of
    True -> return ()
    False -> error "Command wont work."

pause :: IO ()
pause  = do 
  token <- readToken
  result <- touchRequest token "pause" "PUT"
  case result of
    True -> return ()
    False -> error "Command wont work."

playCurrent :: IO ()
playCurrent = do 
  token <- readToken
  result <- touchRequest token "play" "PUT"
  case result of
    True -> return ()
    False -> error "Command wont work."

playSong :: String -> IO ()
playSong songId = return ()

searchSong :: String -> IO ()
searchSong songName  = return ()

showCurrentSong :: IO String
showCurrentSong  = return "Test"

deviceHelp :: IO ()
deviceHelp = putStrLn [r|
  -h              Help 

  list            List available devices
  select          Select first available device
  select DEVICE   Select the device
|]

listDevices :: IO [String]
listDevices  = do
  token <- readToken 
  deviceList <- getDevices token
  let d = fmap device_id deviceList 
  return d
  
selectDevice :: String -> IO ()
selectDevice deviceName = return ()