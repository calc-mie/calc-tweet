{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main(main) where

import Lib
import TwitterAPI
import SlackAPI
import CalcParser.Parser
import CalcParser.Exec

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
 oldcalcweb <- getDirectoryContents srvcalcdir
 -- api key
 botconf <- getAPIkeys
 -- message queue
 msgqueue <- newMVar PostQueue{mentions = V.empty, schedule = V.empty } :: IO (MVar PostQueue)
 -- main
 tlmention <- (\t -> case t of Left  e -> error e
                               Right l -> (gmid_str.Prelude.head) l) <$> getMention T.empty botconf
 monitoring msgqueue tlmention botconf 

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
