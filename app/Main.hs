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
 monitoring (PFData { tl = showTL, dm = showDM }) msgqueue tlmention botconf

showTL :: T.Text -> T.Text -> BotsAPI -> IO(T.Text)
showTL postTarget id conf = if T.null postTarget then return T.empty else do
 postSlack postTarget $ slack conf -- post slack
 sendMessageDiscord postTarget $ discord conf -- post discord
 let msgs = splitN postTarget 140 -- 140 is twitter max size
 start_id <- twfunc (Prelude.head msgs) (T.empty) -- post Timeline
 if (not.Prelude.null.Prelude.tail) msgs then postTweetInReply (Prelude.tail msgs) start_id twfunc else return T.empty
 where
  twfunc :: T.Text -> T.Text -> IO(T.Text)
  twfunc msg id = tweet msg id (twitter conf) >>= (\tl -> return (case tl of Left  e -> error e
                                                                             Right t -> ptl_id_str t))
  splitN :: T.Text -> Int -> [T.Text] 
  splitN raw max = if T.length raw <= max then [raw] else (T.take max raw):splitN (T.drop max raw) max
  postTweetInReply :: [T.Text] -> T.Text -> (T.Text -> T.Text -> IO(T.Text)) -> IO(T.Text)
  postTweetInReply [] _ _            = return (T.empty)
  postTweetInReply (x:xs) nowid func = if T.null nowid then return (T.empty) else do
   nextid <- twfunc x nowid
   postTweetInReply xs nextid func

showDM :: T.Text -> T.Text -> BotsAPI -> IO(T.Text)
showDM msg id conf = if T.null msg then return T.empty else postDM msg id (twitter conf) >> return T.empty
