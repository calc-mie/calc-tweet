{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module SlackAPI (postSlack)where

import Web.Slack
import Web.Slack.WebAPI
import Data.Text
import qualified Data.Text.IO as T
import Control.Monad.Except

slackconf = "/usr/local/calc-tweet/bot/slackbot.conf"

slackBotUser :: IO(Text,Text)
slackBotUser =(\[a,b]->(a,b)).Data.Text.lines<$>T.readFile slackconf

postSlack :: Text -> IO (Either Text ())
postSlack twid = do
 (slackapi,cid) <- slackBotUser
 runExceptT $ chat_postMessage (SlackConfig $ unpack slackapi) (Id cid) twid []

