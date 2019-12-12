{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main(main) where

import Lib
import TwitterAPI
import SlackAPI

import Control.Concurrent
import qualified Data.Text.IO as TIO
import Data.List
import qualified Data.Text as T
import Data.Time
import qualified Data.Vector as V
import System.Directory

main = do
 -- calcweb-post
 oldcalcweb <- getDirectoryContents srvcalcdir
 -- main
 direct_message <- getGetDM
 case direct_message of
  Right dm -> monitoring (setPostData ((getcreated_timestamp . head . getevents) dm, oldcalcweb, [], False)) >> putStrLn "fin"

monitoring :: PostData -> IO PostData
monitoring pd = do
 threadDelay(7*10*1000*1000)
 postdata <- (rtCheck pd >>= remindCheck typeTL)-- monitoring retweeting
 -- monitoring direct message
 directmessage <- getGetDM
 case directmessage of 
  Left err -> monitoring postdata
  Right dm -> if befts pd == (getcreated_timestamp . head . getevents) dm 
               then monitoring postdata
              else do
               pusr <- (TIO.readFile permitconf >>= getUser.T.intercalate (T.pack ",").T.lines)
               case pusr of
                Left err             -> monitoring postdata
                Right permissionuser -> ( do
                 let puser = permissionIndexes ((map sender_idpart) ((getevents) dm)) permissionuser 0
                 cmdCheck (postdata{befts = (getcreated_timestamp . head . getevents) dm }) ((V.fromList.getevents) dm) (
                  case elemIndex (befts pd) (map getcreated_timestamp (getevents dm)) of 
                   Nothing -> (length.map getcreated_timestamp) (getevents dm)
                   Just n  -> (n-1) ) >>= monitoring )

cmdCheck :: PostData -> V.Vector GetMessageCreate -> Int -> IO PostData 
cmdCheck postdata tw n
 | n < 0                 = return postdata
 | otherwise             = 
  case (T.unpack.head.head.map T.words.T.lines.gettext.getmessage_data.getmessage_create) (tw V.! n) of
   "$post"          -> postTweet postdata ((filter ((==sender_idpart (tw V.! n)).sender_idpart).V.toList.V.drop (n+1)) tw) typeTL >>= (\ret -> cmdCheck ret tw (n-1))
   "$print"         -> postTweet postdata ((filter ((==sender_idpart (tw V.! n)).sender_idpart).V.toList.V.drop (n+1)) tw) typeDM >>= (\ret -> cmdCheck ret tw (n-1))
   "$post-calc-web" -> calcWebPost postdata ((V.toList.V.drop (n+1)) tw) typeTL >>= (\ret -> cmdCheck ret tw (n-1))
   "$useradd"       -> userAdd postdata ((V.toList.V.drop (n+1)) tw) >>= (\ret -> cmdCheck ret tw (n-1))
   _                -> cmdCheck postdata tw (n-1)

sender_idpart = getsender_id.getmessage_create

typeDM :: T.Text -> PostData -> [GetMessageCreate] -> IO T.Text
typeDM posttx postdata tw = do
 postDM posttx ((getsender_id.getmessage_create.head) tw)
 return (T.pack "")

typeTL :: T.Text -> PostData -> [GetMessageCreate] -> IO T.Text
typeTL posttx postdata tw = do 
 response <- tweet posttx
 postSlack posttx
 case response of
  Left err -> return (T.pack "")
  Right re -> return (id_str re)

typeTerm :: T.Text -> PostData -> [GetMessageCreate] -> IO T.Text 
typeTerm posttx postdata tw = do
 print posttx
 return (T.pack "")

