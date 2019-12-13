{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module TwitterAPI ( GetDM (..)
                  , GetMessageData (..)
                  , GetMessageCreate (..)
                  , GetEvents (..)
                  , PostTarget (..)
                  , PostDM (..)
                  , PostMessageData (..)
                  , PostMessageCreate (..)
                  , PostEvent (..)
                  , GetTL (..)
                  , PostTL (..)
                  , User (..)
                  , GetMention (..)
                  , GetEntities (..)
                  , GetUrls (..)
                  , getDM
                  , getTL
                  , getUser
                  , getUserTL
                  , getMention
                  , rmTweet
                  , postRT
                  , postDM
                  , tweet
                  , getAPIkeys ) where

import System.IO
import Control.Concurrent
import Control.Exception
import Data.Text
import Data.Text.IO 
import Data.Text.Encoding
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Web.Authenticate.OAuth
import Data.ByteString.Lazy.Internal
import Control.Monad.IO.Class

-- get DM parser
data GetMessageData = GetMessageData { gmd_text :: Text
                   } deriving(Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 } ''GetMessageData)

data GetMessageCreate = GetMessageCreate { gmc_message_data :: GetMessageData
                                         , gmc_sender_id :: Text
                                         } deriving(Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 } ''GetMessageCreate)

data GetEvents = GetEvents { gev_message_create :: GetMessageCreate
                           , gev_created_timestamp :: Text
                           } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 } ''GetEvents)

data GetDM = GetDM { gdm_events :: [GetEvents]
                   } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 } ''GetDM)

-- post DM parser
data PostTarget = PostTarget { ptg_recipient_id :: Text
                             } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 } ''PostTarget)

data PostMessageData = PostMessageData { pmd_text :: Text
                                       } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 } ''PostMessageData)

data PostMessageCreate = PostMessageCreate { pmc_message_data :: PostMessageData
                                           , pmc_target :: PostTarget
                                           } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 } ''PostMessageCreate)

data PostEvent = PostEvent { pev_type :: Text
                           , pev_message_create :: PostMessageCreate
                           } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 } ''PostEvent)

data PostDM = PostDM { pdm_event :: PostEvent
                     } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 } ''PostDM)

--get User parser
data User = User { gur_id_str :: Text
                 , gur_screen_name :: Text } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 }  ''User)

--get TL Parser
data GetTL = GetTL { gtl_text :: Text
                   , gtl_id_str :: Text
                   , gtl_in_reply_to_status_id_str :: Maybe Text
                   , gtl_user :: User } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 } ''GetTL)

--post TL parser
data PostTL = PostTL { ptl_id_str :: Text} deriving (Show)
$(deriveJSON defaultOptions {fieldLabelModifier = Prelude.drop 4 } ''PostTL)

--get Mention parser

data GetUrls = GetUrls { gul_expanded_url :: Text } deriving(Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 }  ''GetUrls)

data GetEntities = GetEntities { gen_urls :: [GetUrls] } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 }  ''GetEntities)

data GetMention = GetMention { gmt_id_str :: Text
                             , gmt_text   :: Text
                             , gmt_entities :: GetEntities
                             , gmt_user   :: User} deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 }  ''GetMention)

getDM :: [String] -> IO (Either String GetDM)
getDM botconf = do
 response <- do
  req <- parseRequest $ "https://api.twitter.com/1.1/direct_messages/events/list.json"
  httpManager req botconf
 return $ eitherDecode $ responseBody response

getMyTweet :: [String] -> IO(Either String [GetTL])
getMyTweet botconf = do
 response <- do
  req <- parseRequest $ "https://api.twitter.com/1.1/statuses/user_timeline.json"
  httpManager req botconf
 return $ eitherDecode $ responseBody response

getTL :: [String] -> IO (Either String [GetTL])
getTL botconf = do
 response <- do
  req <- parseRequest $ "https://api.twitter.com/1.1/statuses/home_timeline.json?count=200"
  httpManager req botconf
 return $ eitherDecode $ responseBody response

getUserTL :: Text -> Text -> [String] -> IO (Either String [GetTL])
getUserTL user_id since_id botconf = do
 response <- do
  req <- parseRequest $ "https://api.twitter.com/1.1/statuses/user_timeline.json?count=200&user_id=" ++ unpack user_id ++
                                                                                         "&since_id=" ++ unpack since_id
  httpManager req botconf
 return $ eitherDecode $ responseBody response

getMention :: Text -> [String] -> IO(Either String [GetMention])
getMention since_id botconf = do
 response <- do
  req <- parseRequest $ "https://api.twitter.com/1.1/statuses/mentions_timeline.json?since_id=" ++ unpack since_id
  httpManager req botconf
 return $ eitherDecode $ responseBody response

getUser :: Text -> [String] -> IO (Either String [User])
getUser screen_name botconf = do
 response <- do
  req <- parseRequest $ "https://api.twitter.com/1.1/users/lookup.json?screen_name="++unpack screen_name
  httpManager req botconf
 return $ eitherDecode $ responseBody response

postRT :: Text -> [String] -> IO ()
postRT twid botconf = do
 req     <- parseRequest $ "https://api.twitter.com/1.1/statuses/retweet/" ++ unpack twid ++ ".json"
 manager <- newManager tlsManagerSettings
 let postReq = urlEncodedBody [("id", encodeUtf8 twid)] req
 httpManager postReq botconf
 return ()

tweet :: Text -> Text -> [String] -> IO (Either String PostTL)
tweet tw twid botconf = do
 responce <- do
  req     <- parseRequest $ "https://api.twitter.com/1.1/statuses/update.json" ++ if Data.Text.null twid then "" 
                                                                                  else "?in_reply_to_status_id=" ++ unpack twid
  let postReq = urlEncodedBody [("status", encodeUtf8 tw)] req
  httpManager postReq botconf
 return $ eitherDecode $ responseBody responce

rmTweet :: Text -> [String] -> IO()
rmTweet twid botconf = do
 req     <- parseRequest $ "https://api.twitter.com/1.1/statuses/destroy.json?id=" ++ unpack twid
 httpManager req botconf
 return ()

postDM :: Text -> Text -> [String] -> IO ()
postDM tw uid botconf = do
 responce <- do
  req <-(\n->n {method = "POST"}) <$>parseRequest "https://api.twitter.com/1.1/direct_messages/events/new.json"  
  let json = PostDM { pdm_event = PostEvent 
                                  { pev_type = "message_create" 
                                  , pev_message_create = PostMessageCreate
                                                         { pmc_message_data = PostMessageData { pmd_text = tw}
                                                         , pmc_target = PostTarget { ptg_recipient_id = uid}}}}
  let postreq = setRequestBodyJSON json req
  httpManager postreq botconf
 return ()

httpManager :: Request -> [String] ->  IO(Response Data.ByteString.Lazy.Internal.ByteString)
httpManager req botconf = do
 (myOAuth, myCredential) <- botuser botconf
 signedReq <- signOAuth myOAuth myCredential req
 manager <- newManager tlsManagerSettings
 Network.HTTP.Conduit.httpLbs signedReq manager

botuser :: [String] -> IO(OAuth,Credential)
botuser botsparameter = do
 let  myOAuth      = newOAuth { oauthServerName     = "api.twitter.com"
                              , oauthConsumerKey    = C.pack(Prelude.head botsparameter)
                              , oauthConsumerSecret = C.pack(botsparameter !! 1)
  }
      myCredential = newCredential (C.pack(botsparameter !! 2)) (C.pack(botsparameter !! 3))
 return (myOAuth, myCredential)

getAPIkeys :: IO [String]
getAPIkeys = do
 hSetEcho stdin False
 apis <- subGetAPI ["API key :", "API secret key :", "Access token :", "Access token secret :"]
 hSetEcho stdin True
 return apis
 where
  subGetAPI :: [String] -> IO[String]
  subGetAPI [] = return []
  subGetAPI (m:messages) = do
   Prelude.putStr m 
   hFlush stdout
   api <- Prelude.getLine 
   Prelude.putChar '\n'
   subGetAPI messages >>= (\res -> return (api:res))

