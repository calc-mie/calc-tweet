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
 --oldcalcweb <- getDirectoryContents srvcalcdir
 -- api key
 twbotconf <- getTwitterAPIKeys
 slackbotconf <- getSlackAPIKeys
 discordbotconf <- getDiscordAPIKeys
 let botconf = BotsAPI { twitter = twbotconf, slack = slackbotconf , discord = Prelude.head discordbotconf}
 -- message queue
 raw <- V.fromList.Prelude.map ((\x-> (V.head x, V.tail x)).V.fromList.commaSep).T.lines <$> TIO.readFile groupsconf
 msgqueue <- newMVar PostQueue{mentions = V.empty, schedule = V.empty, pqGroups = raw} :: IO (MVar PostQueue)
 -- main
 tlmention <- (\t -> case t of Left  e -> error e
                               Right l -> (gmt_id_str.Prelude.head) l) <$> getMention (T.singleton '1') (twitter botconf)
 monitoring (Postfunc { tl = showTL, dm = showDM }) msgqueue tlmention botconf

showTL :: T.Text -> T.Text -> [String] -> IO(T.Text)
showTL msg id conf = tweet msg id conf >>= (\tl -> return (case tl of Left  e -> error e
                                                                      Right t -> ptl_id_str t))

showDM :: T.Text -> T.Text -> [String] -> IO(T.Text)
showDM msg id conf = postDM msg id conf >> return T.empty


 -- get mentions timeline
 -- main
-- direct_message <- getGetDM
-- case direct_message of
--  Right dm -> monitoring (setPostData ((getcreated_timestamp . head . getevents) dm, oldcalcweb, [], False)) >> putStrLn "fin"


--typeDM :: T.Text -> PostData -> GetMention -> [String] -> IO T.Text
--typeDM posttx postdata tw botconf = do
-- postDM posttx ((getsender_id.getmessage_create.head) tw) botconf
-- return (T.pack "")
--
--typeTL :: T.Text -> PostData -> GetMention -> [String] -> IO T.Text
--typeTL posttx postdata tw botconf = do 
-- response <- tweet posttx botconf
-- postSlack posttx
-- case response of
--  Left err -> return (T.pack "")
--  Right re -> return (id_str re)
--
--typeTerm :: T.Text -> PostData -> GetMention -> [String] -> IO T.Text 
--typeTerm posttx postdata tw botconf = do
-- print posttx
-- return (T.pack "")
