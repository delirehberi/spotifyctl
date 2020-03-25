{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import SpotifyCommand
import           Data.Maybe

main :: IO ()
main = do
  flags <- getArgs
  command flags 
  

command :: [String] -> IO ()
command ["-h"] = help
command ["auth"] = auth
command ["refresh"] = refresh
command ["next"] = nextSong
command ["prev"] = previousSong
command ["pause"] = pause
command ["play"] = playCurrent
command ["current"] = putStrLn =<< showCurrentSong
command ("play":xs) = playSong $ head xs 
command ("device":xs) = deviceCommand xs
command ("search":xs) = searchSong $ head xs 
command [] = help 

deviceCommand :: [String] -> IO ()
deviceCommand ["-h"] = deviceHelp
deviceCommand ["list"] = putStrLn =<< (unlines  <$>  listDevices)
deviceCommand ["select"] = selectDevice =<< (head <$> listDevices)
deviceCommand ("select":xs) = selectDevice $ head xs 
deviceCommand [] = deviceHelp 