{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main(main) where

import Lib
import TwitterAPI
import qualified Data.Text.IO as T
import Data.Text
import System.Directory

-- post test data for debug

--posttx :: PostEvent
--posttx = PostEvent {
-- postevent = PostMessageCreate{
--  posttype = "message_create" ,
--  postmessage_create = PostMessageData { 
--   posttarget = PostRecipient {
--    postrecipient_id = "862599998147403776"
--   } ,
--   postmessage_data = PostDM {
--    posttext = "This is test dm."
--   }
--  }
-- }
--}

main = do
 --setup
 puser <- T.readFile permitconf
 if Data.Text.null puser then 
  (do 
   putStrLn "input userid"
   username <- T.getLine
   userid <- getUser username
   case userid of
    Left err -> putStrLn "error"
    Right ui -> T.appendFile permitconf ((gid_str.Prelude.head)ui))
  else putStrLn "start"

 -- calcweb-post
 oldcalcweb <- getDirectoryContents srvcalcdir
 -- main
 direct_message <- getGetDM
 case direct_message of
  Right dm -> monitoring (setPostData ([],oldcalcweb,[])) dm >> putStrLn "fin"

 --for debug
 --postTestDM $ posttx
 -- tweet "test"
 -- timeline <- getGetDM
 -- print timeline
 --case direct_message of
 -- Left err -> putStrLn "error"
 -- Right tl -> (print . getcreated_timestamp . Prelude.head . getevents) tl 
-- user <- (getUser (pack "calc_mie"))
-- case user of 
--  Left err -> print "error"
--  Right us -> T.putStrLn ((gid_str.Prelude.head) us)



