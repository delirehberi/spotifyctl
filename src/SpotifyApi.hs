{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module SpotifyApi where
import Network.HTTP.Types
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import GHC.Generics
import Network.Wreq
import qualified Network.Wreq as WR

import Control.Lens
import Data.Aeson.Lens (_String, key, nth)
import Data.Aeson 
import qualified Data.ByteString.Base64 as B64
import System.Environment (getEnv)
import           Data.List (isPrefixOf)
import Control.Exception as E
import Data.Maybe (fromMaybe)

baseurl :: String
baseurl = "https://api.spotify.com/v1"

spotifyKey :: IO OAuthInfo
spotifyKey = do
  client_id <- getEnv "CLIENTID"
  client_secret <- getEnv "CLIENTSECRET"

  return OAuthInfo { 
    oauthClientId = client_id
  , oauthClientSecret =  client_secret 
  , oauthCallback =  "http://127.0.0.1:9988/oauthCallback"
  , oauthOAuthorizeEndpoint =  "https://accounts.spotify.com/authorize" 
  , oauthAccessTokenEndpoint = "https://accounts.spotify.com/api/token"
}


data OAuthInfo = OAuthInfo {
    oauthClientId :: String
  , oauthClientSecret :: String
  , oauthCallback :: String
  , oauthOAuthorizeEndpoint :: String
  , oauthAccessTokenEndpoint :: String
}

data Token = Token {
    access_token :: String
  , token_type :: String
  , scope :: String
  , expires_in :: Int
  , refresh_token :: Maybe String
} deriving (Generic, Show)
  

data Device = Device {
  device_id :: String
  , device_is_active :: Bool
  , device_is_private_session :: Bool
  , device_is_restricted :: Bool
  , device_name :: String
  , device_type :: String
  , device_volume_percent :: Int
} deriving (Generic, Show)

data DeviceList = DeviceList {
  devices :: [Device]
}deriving (Generic, Show)

instance FromJSON DeviceList
instance FromJSON Token
instance ToJSON Token
instance FromJSON Device where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = prefixRemover "device_"
    }

prefixRemover :: String -> String -> String
prefixRemover prefix val | prefix `isPrefixOf` val = drop (length prefix) val
                         | otherwise               = ""


queryString :: String -> (String, String) -> String
queryString acc (k,v) = acc ++ "&" ++ k ++ "=" ++ encodedV v
  where
    encodedV v = BSU.toString $ urlEncode True (BSU.fromString v)

generateAuthUrl :: OAuthInfo -> String
generateAuthUrl authKey = 
  let  
    queryParameters = [
          ("client_id", oauthClientId authKey)
        , ("response_type", "code")
        , ("redirect_uri", oauthCallback authKey)
        , ("scope", "user-read-playback-state streaming user-read-currently-playing app-remote-control")
        , ("state", "random-state-here")
        ]
  in
    oauthOAuthorizeEndpoint authKey ++ "?" ++  foldl queryString "" queryParameters 
       

fetchToken :: OAuthInfo -> BSL.ByteString -> IO (Response BSL.ByteString)
fetchToken key code = do
  let 
    params = [
        ("grant_type"::BS.ByteString) := ("authorization_code"::String)
      , "code" := code
      , "redirect_uri" := oauthCallback key
      ]
    authorization = BS.append ("Basic "::BS.ByteString) (hashedClientData key)
    opts = defaults & header "Authorization" .~ [authorization]
  postWith opts ( oauthAccessTokenEndpoint key) params

saveToken :: BSL.ByteString -> IO ()
saveToken = BSL.writeFile "/home/delirehberi/.spotifyctl"

addRefreshToken :: Response Token -> Token -> Token
addRefreshToken newToken oldToken = Token {
  access_token = access_token (newToken ^. responseBody),
  token_type = token_type (newToken ^. responseBody),
  scope = scope (newToken ^. responseBody),
  expires_in = expires_in (newToken ^. responseBody),
  refresh_token = refresh_token oldToken 
  }

refreshToken :: Token -> IO Token
refreshToken token = do
  spkey <- spotifyKey
  let 
    params = [
      ("grant_type"::BS.ByteString) := ("refresh_token"::String)
      , "refresh_token" := fromMaybe "" (refresh_token token)
      ]
    authorization = BS.append ("Basic "::BS.ByteString) (hashedClientData spkey)
    opts = defaults & header "Authorization" .~ [authorization]
  
  r <- asJSON =<< postWith opts (oauthAccessTokenEndpoint spkey) params
  let newToken =  addRefreshToken r token
  saveToken $ encode newToken
  readToken

touchRequest :: Token -> String -> String -> IO Bool
touchRequest token command  method= do
  r <- customMethodWith method (authOpts token) (apiUrl "me/player/" ++ command)
  case r ^. responseStatus . WR.statusCode of 
    200 -> return True
    202 -> return True
    _ -> return False


getDevices :: Token -> IO [Device]
getDevices token = do
  r <- asJSON =<< getWith (authOpts token) (apiUrl "me/player/devices")
  return (devices (r ^. responseBody))

hashedClientData :: OAuthInfo -> BS.ByteString
hashedClientData key = B64.encode $ BSU.fromString $ oauthClientId key ++ ":" ++ oauthClientSecret key

authOpts ::Token -> WR.Options
authOpts token = defaults & header "Authorization" .~ [authorization]
    where
      authorization = BS.append ("Bearer "::BS.ByteString) (BSU.fromString $ access_token token)

apiUrl::String->String
apiUrl endpoint = baseurl ++ "/" ++ endpoint

homedir :: IO String
homedir = do 
  user <- getEnv "USER"
  return ("/home/" ++ user ++ "/")

readToken :: IO Token
readToken = do 
  home <- homedir
  content <- BSL.readFile (home ++ ".spotifyctl")
  let decoded = decode content :: Maybe Token
  case decoded of 
    Just x -> return x
    Nothing -> error $ "Can't read the file in " ++ home 