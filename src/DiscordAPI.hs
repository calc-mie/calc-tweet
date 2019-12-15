{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module DiscordAPI where

import TwitterAPI (getAPIkeys)

import qualified Data.Text as T
import Data.Text.Encoding
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Web.Authenticate.OAuth
import Data.ByteString.Lazy.Internal
import System.IO

-- import Network.Discord

getDiscordAPIKeys :: IO [String]
getDiscordAPIKeys = do
 hSetEcho stdin False
 System.IO.putStrLn "======  discord key ======"
 apis <- getAPIkeys ["Token :", "ChannelID :"]
 hSetEcho stdin True
 return apis

sendMessageDiscord :: T.Text -> [String] -> IO ()
sendMessageDiscord content conf = return ()
-- let message = MessageCreateEvent {}
 
-- let (channel, token) = (\[a, b] -> (a,b)) conf
-- req <- parseRequest $ "https://discordapp.com/api/channels/" ++ channel ++ "/messages"
-- let postReq = urlEncodedBody [("content", encodeUtf8 content)] req
-- return ()

