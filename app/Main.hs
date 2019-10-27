{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main(main) where

import Lib
import TwitterAPI
import SlackAPI

import Control.Concurrent
import qualified Data.Text.IO as TIO
import qualified Data.List as L
import qualified Data.Text as T
import Data.Time
import qualified Data.Vector as V
import System.Directory
import System.IO
import Control.Exception

emptyint = 1*1000*1000 {- 1 second -}
mentiont = 12*1000*1000 {-12 second -}

main = do
 -- calcweb-post
 oldcalcweb <- getDirectoryContents srvcalcdir
 -- api key
 hSetEcho stdin False
 botconf <- getAPIkeys ["API key :", "API secret key :", "Access token :", "Access token secret :"]
 hSetEcho stdin True
 -- message queue
 msgqueue <- newMVar [] :: IO (MVar V.Vector GetMention)
 -- main
 tlmention <- (\t -> case t of Left  e -> error e
                               Right l -> (gmid_str.L.last) l) <$> getMention (pack "") botconf
 monitoring msgqueue (setPostData ((gmid_str.L.head) tlmention) oldcalcweb, [], Flase) botconf
 -- get mentions timeline
 -- main
-- direct_message <- getGetDM
-- case direct_message of
--  Right dm -> monitoring (setPostData ((getcreated_timestamp . head . getevents) dm, oldcalcweb, [], False)) >> putStrLn "fin"

 
-- case directmessage of 
--  Left err -> monitoring postdata
--  Right dm -> if befts pd == (getcreated_timestamp . head . getevents) dm 
--               then monitoring postdata
--              else do
--               pusr <- (TIO.readFile permitconf >>= getUser.T.intercalate (T.pack ",").T.lines)
--               case pusr of
--                Left err             -> monitoring postdata
--                Right permissionuser -> ( do
--                 let puser = permissionIndexes ((map sender_idpart) ((getevents) dm)) permissionuser 0
--                 cmdCheck (postdata{befts = (getcreated_timestamp . head . getevents) dm }) ((V.fromList.getevents) dm) (
--                  case elemIndex (befts pd) (map getcreated_timestamp (getevents dm)) of 
--                   Nothing -> (length.map getcreated_timestamp) (getevents dm)
--                   Just n  -> (n-1) ) >>= monitoring )

   
--cmdCheck :: PostData -> V.Vector GetMessageCreate -> Int -> IO PostData 
--cmdCheck postdata tw n
-- | n < 0                 = return postdata
-- | otherwise             = 
--  case (T.unpack.head.head.map T.words.T.lines.gettext.getmessage_data.getmessage_create) (tw V.! n) of
--   "$post"          -> postTweet postdata ((filter ((==sender_idpart (tw V.! n)).sender_idpart).V.toList.V.drop (n+1)) tw) typeTL >>= (\ret -> cmdCheck ret tw (n-1))
--   "$print"         -> postTweet postdata ((filter ((==sender_idpart (tw V.! n)).sender_idpart).V.toList.V.drop (n+1)) tw) typeDM >>= (\ret -> cmdCheck ret tw (n-1))
--   "$post-calc-web" -> calcWebPost postdata ((V.toList.V.drop (n+1)) tw) typeTL >>= (\ret -> cmdCheck ret tw (n-1))
--   "$useradd"       -> userAdd postdata ((V.toList.V.drop (n+1)) tw) >>= (\ret -> cmdCheck ret tw (n-1))
--   _                -> cmdCheck postdata tw (n-1)
--
--sender_idpart = getsender_id.getmessage_create

typeDM :: T.Text -> PostData -> GetMention -> [String] -> IO T.Text
typeDM posttx postdata tw botconf = do
 postDM posttx ((getsender_id.getmessage_create.head) tw) botconf
 return (T.pack "")

typeTL :: T.Text -> PostData -> GetMention -> [String] -> IO T.Text
typeTL posttx postdata tw botconf = do 
 response <- tweet posttx botconf
 postSlack posttx
 case response of
  Left err -> return (T.pack "")
  Right re -> return (id_str re)

typeTerm :: T.Text -> PostData -> GetMention -> [String] -> IO T.Text 
typeTerm posttx postdata tw botconf = do
 print posttx
 return (T.pack "")
