{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main(main) where

import Lib
import TwitterAPI
import SlackAPI
import DiscordAPI
import Parser
import Exec

import Control.Concurrent
import qualified Data.Text.IO as TIO
import qualified Data.List as L
import qualified Data.Text as T
import Data.Time
import qualified Data.Vector as V
import System.Directory
import System.IO
import Control.Exception

main = do
 -- calcweb-post
 -- oldcalcweb <- getDirectoryContents srvcalcdir
 -- api key
 twbotconf <- getTwitterAPIKeys
 slackbotconf <- getSlackAPIKeys
 discordbotconf <- getDiscordAPIKeys
 let botconf = BotsAPI { twitter = twbotconf, slack = slackbotconf , discord = Prelude.head discordbotconf}
 -- message queue
 raw <- V.fromList.Prelude.map ((\x-> (V.head x, V.tail x)).V.fromList.commaSep).T.lines <$> TIO.readFile groupsconf
 msgqueue <- newMVar PostQueue{mentions = V.empty, schedule = V.empty, pqGroups = raw } :: IO (MVar PostQueue)
 -- main
 tlmention <- (\t -> case t of Left  e -> error e
                               Right l -> (gmt_id_str.Prelude.head) l) <$> getMention (T.singleton '1') (twitter botconf)
 monitoring (Postfunc {tl = showTL, dm = showDM}) msgqueue tlmention botconf 

showTL :: T.Text -> T.Text -> [String] -> IO(T.Text)
showTL msg id conf = do
 putStrLn "======= test showTL ========"
 print msg
 print id 
 putStrLn "======= end showTL =========="
 return T.empty

showDM = showTL

--main = do
-- print "test startiing!!!"
-- -- calcweb-post
-- let oldcalcweb = []
-- -- main
-- direct_message <- getGetDMtest
-- print "popopopo"
-- case direct_message of
--  Right dm -> monitoring (setPostData ((getcreated_timestamp . Prelude.head . getevents) dm, oldcalcweb, [], False)) >> putStrLn "fin"
--
--monitoring :: PostData -> IO PostData
--monitoring pd = do
-- threadDelay(3*1000*1000)
-- print (befts pd)
-- postdata <- (rtCheck pd >>= remindCheck typeTerm)-- monitoring retweeting
-- -- monitoring direct message
-- directmessage <- getGetDMtest
-- case directmessage of 
--  Left err -> monitoring postdata
--  Right dm -> if befts pd == (getcreated_timestamp . Prelude.head . getevents) dm 
--               then monitoring postdata
--              else do
--               -- pusr <- (TIO.readFile permitconf >>= getUser.T.intercalate (T.pack ",").T.lines)
--               print dm
--               pusr <- getUserTest
--               case pusr of
--                Left err             -> monitoring postdata
--                Right permissionuser -> ( do
--                 print "monitoring"
--                 let puser = permissionIndexes ((Prelude.map (getsender_id.getmessage_create)) ((getevents) dm)) permissionuser 0
--                 cmdCheck (postdata{befts = (getcreated_timestamp . Prelude.head . getevents) dm }) ((V.fromList.getevents) dm) (
--                  case elemIndex (befts pd) (Prelude.map getcreated_timestamp (getevents dm)) of 
--                   Nothing -> (length.Prelude.map getcreated_timestamp) (getevents dm)
--                   Just n  -> (n-1) ) >>= monitoring)
--
--cmdCheck :: PostData -> V.Vector GetMessageCreate -> Int -> IO PostData 
--cmdCheck postdata tw n
-- | n < 0                 = return postdata
-- | otherwise              = 
--  case (T.unpack.Prelude.head.Prelude.head.Prelude.map T.words.T.lines.gettext.getmessage_data.getmessage_create) (tw V.! n) of
--   "$post"          -> print "post" >> postTweet postdata ((V.toList.V.drop (n+1)) tw) typeTerm >>= (\ret -> cmdCheck ret tw (n-1))
--   "$print"         -> print "print" >>postTweet postdata ((V.toList.V.drop (n+1)) tw) typeTerm >>= (\ret -> cmdCheck ret tw (n-1))
--   "$post-calc-web" -> print "post-calc" >>calcWebPost postdata ((V.toList.V.drop (n+1)) tw) typeTerm >>= (\ret -> cmdCheck ret tw (n-1))
--   "$useradd"       -> print "post-user" >>userAdd postdata ((V.toList.V.drop (n+1)) tw) >>= (\ret -> cmdCheck ret tw (n-1))
--   _                -> print "non" >> cmdCheck postdata tw (n-1)
--
--
--typeDM :: T.Text -> PostData -> [GetMessageCreate] -> IO T.Text
--typeDM posttx postdata tw = do
-- postDM posttx ((getsender_id.getmessage_create.Prelude.head) tw)
-- return (T.pack "")
--
--typeTL :: T.Text -> PostData -> [GetMessageCreate] -> IO T.Text
--typeTL posttx postdata tw = do 
-- response <- tweet posttx
-- postSlack posttx
-- case response of
--  Left err -> return (T.pack "")
--  Right re -> return (id_str re)
--
--typeTerm :: T.Text -> PostData -> [GetMessageCreate] -> IO T.Text 
--typeTerm posttx postdata tw = do
-- print posttx
-- return (T.pack "")
--
--getGetDMtest :: IO (Either String GetEvents)
--getGetDMtest = do
-- messg <- T.lines <$> TIO.readFile "./test/test.txt"
-- return(Right (GetEvents { getevents = loop messg}))
--  where
--   loop msg = if null msg then [] 
--    else GetMessageCreate { getcreated_timestamp = (head.T.words.head) msg
--                          , getmessage_create = GetMessageData { getsender_id = "0" 
--                                                               , getmessage_data = GetDM { gettext = (T.unwords.tail.T.words.head) msg}}}:loop (tail msg)
--
--getUserTest :: IO (Either String [User])
--getUserTest = return ( Right [User{gid_str = "0"}])
