{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module TwitterAPI ( permitconf
                  , noticetempconf
                  , calcwebtempconf
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
                  , PostTL (..)
                  , User (..)
                  , getMention (..)
                  , getGetDM
                  , getTL
                  , getUser
                  , postRT
                  , postDM
                  , tweet
                  , getAPIkeys ) where

import Control.Concurrent
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

permitconf = "/usr/local/calc-tweet/permissionuser.conf"
noticetempconf = "/usr/local/calc-tweet/temp/notice.conf"
calcwebtempconf = "/usr/local/calc-tweet/temp/web.conf"

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

data PostTL = PostTL { id_str :: Text} deriving (Show)
$(deriveJSON defaultOptions ''PostTL)

--get User parser
data User = User { gid_str :: Text } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 1 }  ''User)

--get Mention parser
data GetMention = GetMention { gmid_str :: Text
                             , gmtext   :: Text
                             , gmuser   :: User} deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 2 }  ''User)

getGetDM :: [String] -> IO (Either String GetEvents)
getGetDM botconf = do
 response <- do
  req <- parseRequest  "https://api.twitter.com/1.1/direct_messages/events/list.json"
  httpManager req botconf
 return $ eitherDecode $ responseBody response

getMyTweet :: [String] -> IO(Either String [Tweet])
getMyTweet botconf = do
 response <- do
  req <- parseRequest  "https://api.twitter.com/1.1/statuses/user_timeline.json"
  httpManager req botconf
 return $ eitherDecode $ responseBody response

getTL :: [String] -> IO (Either String [Tweet])
getTL botconf = do
 response <- do
  req <- parseRequest  "https://api.twitter.com/1.1/statuses/home_timeline.json?count=1"
  httpManager req botconf
 return $ eitherDecode $ responseBody response

getMention :: [String] -> IO(Either String [GetMention])
getMention botconf = do
 response <- do
  req <- parseRequest  "https://api.twitter.com/1.1/statuses/mentions_timeline.json?"
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

tweet :: Text -> [String] -> IO (Either String PostTL)
tweet tw botconf = do
 responce <- do
  req     <- parseRequest "https://api.twitter.com/1.1/statuses/update.json"
  let postReq = urlEncodedBody [("status", encodeUtf8 tw)] req
  httpManager postReq botconf
 return $ eitherDecode $ responseBody responce

postDM :: Text -> Text -> [String] -> IO ()
postDM tw un botconf = do
 responce <- do
  req <-(\n->n {method = "POST"}) <$>parseRequest "https://api.twitter.com/1.1/direct_messages/events/new.json"  
  let json = PostEvent { postevent = PostMessageCreate 
                                    { posttype = "message_create" 
                                    , postmessage_create = PostMessageData 
                                                            { postmessage_data = PostDM { posttext = tw}
                                                            , posttarget = PostRecipient { postrecipient_id = un}}}}
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

getAPIkeys :: [String] -> IO [String]
getAPIkeys [] = return []
getAPIkeys (m:messages) = do
 Prelude.putStr m 
 hFlush stdout
 api <- Prelude.getLine 
 Prelude.putChar '\n'
 getAPIkeys messages >>= (\res -> return (api:res))

