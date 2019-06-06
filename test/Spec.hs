{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main(main) where

import Lib
import TwitterAPI
import SlackAPI

import Control.Concurrent
import qualified Data.Text.IO as T
import Data.List
import Data.Text
import Data.Time
import System.Directory

main = do
 -- calcweb-post
 -- oldcalcweb <- getDirectoryContents srvcalcdir
 -- main
 direct_message <- getGetDM
 case direct_message of
  Right dm -> monitoring (setPostData ([],[],[], False)) dm >> putStrLn "fin"

monitoring :: PostData -> GetEvents -> IO PostData
monitoring pd befdm= do
 threadDelay(3*30*1000*1000)
 postdata <- (rtCheck pd >>= remindCheck typeTerm)-- monitoring retweeting
 -- monitoring direct message
 directmessage <- getGetDM
 case directmessage of 
  Left err -> error err
  Right dm -> if (getcreated_timestamp . Prelude.head . getevents) befdm == (getcreated_timestamp . Prelude.head . getevents) dm 
               then monitoring postdata dm 
              else do
               pusr <- (T.readFile permitconf >>= getUser.Data.Text.intercalate (pack ",").Data.Text.lines)
               case pusr of
                Left err             -> error err
                Right permissionuser -> case elemIndex ((getcreated_timestamp . Prelude.head . getevents) befdm) (Prelude.map getcreated_timestamp (getevents dm)) of 
                 Nothing -> monitoring postdata dm
                 Just n  -> do
                  let puser = permissionIndexes ((Prelude.map (getsender_id.getmessage_create)) ((Prelude.take n.getevents) dm)) permissionuser 0
                  notices <- makeNotice postdata ((Prelude.reverse.getPermitFromIndex puser.Prelude.take n.getevents) dm)
                  monitoring notices dm 

makeNotice :: PostData -> [GetMessageCreate] -> IO PostData 
makeNotice postdata tw 
 | Prelude.null tw        = return postdata 
 | Prelude.length tw > 20 = return (setPostData([], calcweb postdata, schedule postdata, noon postdata)) 
 | otherwise              = 
  case (unpack.Prelude.head.Prelude.head.Prelude.map Data.Text.words.Data.Text.lines.gettext.getmessage_data.getmessage_create.Prelude.head) tw of
   "$notice"        -> makeNotice (nextMakeNotice postdata tw "notice") (Prelude.tail tw)
   "$time"          -> makeNotice (nextMakeNotice postdata tw "time") (Prelude.tail tw)
   "$date"          -> makeNotice (nextMakeNotice postdata tw "date") (Prelude.tail tw)
   "$locale"        -> makeNotice (nextMakeNotice postdata tw "locale") (Prelude.tail tw)
   "$clear"         -> makeNotice (setPostData ([], calcweb postdata, schedule postdata, noon postdata)) (Prelude.tail tw)
   "$post"          -> postTweet postdata tw typeTerm >>= (\ret -> makeNotice ret (Prelude.tail tw))
   "$print"         -> postTweet postdata tw typeTerm >>= (\ret -> makeNotice ret (Prelude.tail tw))
   "$post-calc-web" -> calcWebPost postdata tw typeTerm >>= (\ret -> makeNotice ret (Prelude.tail tw))
   "$useradd"       -> userAdd postdata tw >>= (\ret -> makeNotice ret (Prelude.tail tw))
   _                -> makeNotice postdata (Prelude.tail tw)


typeDM :: Text -> PostData -> [GetMessageCreate] -> IO Text
typeDM posttx postdata tw = do
 postDM posttx ((getsender_id.getmessage_create.Prelude.head) tw)
 return (pack "")

typeTL :: Text -> PostData -> [GetMessageCreate] -> IO Text
typeTL posttx postdata tw = do 
 response <- tweet posttx
 postSlack posttx
 case response of
  Left err -> return (pack "")
  Right re -> return (id_str re)

typeTerm :: Text -> PostData -> [GetMessageCreate] -> IO Text 
typeTerm posttx postdata tw = do
 print posttx
 return (pack "")

