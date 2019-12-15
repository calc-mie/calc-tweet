{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module SlackAPI where

import TwitterAPI (getAPIkeys)
import Web.Slack
import Web.Slack.WebAPI
import Data.Text
import qualified Data.Text.IO as T
import Control.Monad.Except
import System.IO

getSlackAPIKeys :: IO [String]
getSlackAPIKeys = do
 hSetEcho stdin False
 System.IO.putStrLn "====== slack api key ======"
 apis <- getAPIkeys ["API Token :", "Channel :"]
 hSetEcho stdin True
 return apis

postSlack :: Text -> [String] -> IO (Either Text ())
postSlack twid conf = do
 let (slackapi,cid) = (\[a, b]->(a,b)) conf
 runExceptT $ chat_postMessage (SlackConfig $ slackapi) (Id (pack cid)) twid []
