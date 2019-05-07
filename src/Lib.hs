{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module Lib ( permitconf
           , templateconf
           , botconf
           , GetDM (..)
           , GetMessageData (..)
           , GetMessageCreate (..)
           , GetEvents (..)
           , PostRecipient (..)
           , PostDM (..)
           , PostMessageData (..)
           , PostMessageCreate (..)
           , PostEvent (..)
           , Tweet (..)
           , User (..)
           , getGetDM
           , getTL
           , getUser
           , tweet) where

import Control.Concurrent
import Data.Text
import Data.Text.IO 
import Data.Text.Encoding
import Data.Aeson {- perser -}
import Data.Aeson.TH
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Network.HTTP.Conduit
import Web.Authenticate.OAuth

permitconf = "/usr/local/calc-tweet/permissionuser.conf"
templateconf = "/usr/local/calc-tweet/template.conf"
botconf = "/usr/local/calc-tweet/bot.conf"

-- get DM parser
data GetDM = GetDM { gettext :: Text
                    } deriving(Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 3 } ''GetDM)

data GetMessageData = GetMessageData { getmessage_data :: GetDM
                                     , getsender_id :: Text
                                      } deriving(Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 3 } ''GetMessageData)

data GetMessageCreate = GetMessageCreate { getmessage_create :: GetMessageData
                                         , getcreated_timestamp :: Text
                                          } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 3 } ''GetMessageCreate)

data GetEvents = GetEvents { getevents :: [GetMessageCreate]
                            } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 3 } ''GetEvents)

-- post DM parser
data PostRecipient = PostRecipient { postrecipient_id :: Text
                                    } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 } ''PostRecipient)

data PostDM = PostDM { posttext :: Text
                      } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 } ''PostDM)

data PostMessageData = PostMessageData { postmessage_data :: PostDM 
                                       , posttarget :: PostRecipient
                                        } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 } ''PostMessageData)

data PostMessageCreate = PostMessageCreate { posttype :: Text
                                           , postmessage_create :: PostMessageData
                                           } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 } ''PostMessageCreate)

data PostEvent = PostEvent { postevent :: PostMessageCreate
                              } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 } ''PostEvent)

--post TL parser
data Tweet = Tweet { text :: Text} deriving (Show)
$(deriveJSON defaultOptions  ''Tweet)

--get User parser
data User = User { gid_str :: Text } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 1 }  ''User)

getGetDM :: IO (Either String GetEvents)
getGetDM = do
 response <- do
  req <- parseRequest $ "https://api.twitter.com/1.1/direct_messages/events/list.json"
  (myOAuth, myCredential) <- botuser
  signedReq <- signOAuth myOAuth myCredential req
  manager   <- newManager tlsManagerSettings
  httpLbs signedReq manager
 return $ eitherDecode $ responseBody response

tweet :: Text -> IO ()
tweet tw = do
 req     <- parseRequest "https://api.twitter.com/1.1/statuses/update.json"
 manager <- newManager tlsManagerSettings
 let postReq = urlEncodedBody [("status", encodeUtf8 tw)] req
 (myOAuth, myCredential) <- botuser
 signedReq <- signOAuth myOAuth myCredential postReq
 httpLbs signedReq manager
 return ()

getTL :: IO (Either String [Tweet])
getTL = do
 response <- do
  req <- parseRequest $ "https://api.twitter.com/1.1/statuses/home_timeline.json?count=1"
  (myOAuth, myCredential) <- botuser
  signedReq <- signOAuth myOAuth myCredential req
  manager   <- newManager tlsManagerSettings
  httpLbs signedReq manager
 return $ eitherDecode $ responseBody response

getUser :: Text -> IO (Either String [User])
getUser screen_name = do
 response <- do
  req <- parseRequest $ "https://api.twitter.com/1.1/users/lookup.json?screen_name="++(unpack screen_name)
  (myOAuth, myCredential) <- botuser
  signedReq <- signOAuth myOAuth myCredential req
  manager   <- newManager tlsManagerSettings
  httpLbs signedReq manager
 return $ eitherDecode $ responseBody response


botuser :: IO((OAuth,Credential))
botuser = do
 botsparameter <- Prelude.lines <$> Prelude.readFile botconf
 let  myOAuth      = newOAuth { oauthServerName     = "api.twitter.com"
                              , oauthConsumerKey    = C.pack(botsparameter !! 0)
                              , oauthConsumerSecret = C.pack(botsparameter !! 1)
  }
      myCredential = newCredential (C.pack(botsparameter !! 2)) (C.pack(botsparameter !! 3))
 return (myOAuth, myCredential)

