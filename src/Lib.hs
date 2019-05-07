module Lib ( PostData (..)
           , monitoring
           , setPostData)where

import TwitterAPI
import Control.Concurrent
import Data.List
import qualified Data.Text.IO as T
import Data.Text
--import System.Directory

data PostData = PostData { sendtext :: [(Text, Text)]
                         , calcweb :: String
                         } deriving (Show)

monitoring :: PostData -> GetEvents -> IO(PostData)
monitoring postdata befdm= do
 threadDelay(61*1000*1000) -- 1minits
 directmessage <- getGetDM
 case directmessage of 
  Left err -> error err
  Right dm -> do
   if (getcreated_timestamp . Prelude.head . getevents) befdm == (getcreated_timestamp . Prelude.head . getevents) dm then  monitoring postdata dm else do
    permissionuser <- Data.Text.lines <$> T.readFile permitconf-- idが許可された人かどうかを確認する.そうじゃなかったらmonitoring postdataでloop
    case elemIndex ((getcreated_timestamp . Prelude.head . getevents) befdm) (Prelude.map (getcreated_timestamp) (getevents dm)) of 
     Nothing -> monitoring postdata dm
     Just n -> do
      let puser = permissionIndexes ((Prelude.reverse.Prelude.map (getsender_id.getmessage_create)) ((Prelude.take n.getevents) dm)) permissionuser 0
      notices <- makeNotice postdata ((Prelude.reverse.Prelude.map (gettext . getmessage_data . getmessage_create)) ((getPermitFromIndex puser.Prelude.take n.getevents) dm))
      monitoring notices dm

permissionIndexes :: [Text] -> [Text] -> Int -> [Int]
permissionIndexes dm puser index = if Prelude.null dm then [] else 
 if elem (Prelude.head dm) puser then index:(permissionIndexes (Prelude.tail dm) puser (index + 1))
 else permissionIndexes (Prelude.tail dm) puser (index + 1)

getPermitFromIndex :: [Int] -> [GetMessageCreate] -> [GetMessageCreate]
getPermitFromIndex ind mcs = if Prelude.null ind then [] else ((mcs!!(Prelude.head ind)):(getPermitFromIndex (Prelude.tail ind) mcs))

makeNotice :: PostData -> [Text] -> IO(PostData) 
makeNotice postdata tw = do
 if Prelude.null tw then return postdata else if Prelude.length tw > 20 then return (setPostData([],(calcweb postdata))) else
  case ((unpack.Prelude.head.Prelude.head.Prelude.map Data.Text.words.Data.Text.lines.Prelude.head)tw) of
   "$notice"        -> makeNotice (setPostData(((pack "notice",(Data.Text.drop 8.Prelude.head) tw):(sendtext postdata)), (calcweb postdata))) (Prelude.tail tw)
   "$time"          -> makeNotice (setPostData(((pack "time",(Data.Text.drop 6.Prelude.head) tw):(sendtext postdata)),(calcweb postdata))) (Prelude.tail tw)
   "$date"          -> makeNotice (setPostData(((pack "date",(Data.Text.drop 6.Prelude.head) tw):(sendtext postdata)),(calcweb postdata))) (Prelude.tail tw)
   "$locale"        -> makeNotice (setPostData(((pack "locale",(Data.Text.drop 8.Prelude.head) tw):(sendtext postdata)),(calcweb postdata))) (Prelude.tail tw)
   "$clear"         -> makeNotice (setPostData([],(calcweb postdata))) (Prelude.tail tw)
   "$post"          -> postTweet postdata tw
--   "$post-calc-web" -> calcWebPost postdata tw
   "$useradd"       -> userAdd postdata tw
   otherwise        -> makeNotice postdata (Prelude.tail tw)

setPostData :: ([(Text,Text)],String) -> PostData
setPostData (sendtx, web) = PostData { sendtext = sendtx, calcweb = web }

postTweet :: PostData -> [Text] -> IO (PostData)
postTweet postdata tw = case completePostData postdata (Prelude.map pack ["notice","time","date","locale"]) of 
 False -> makeNotice (setPostData (sendtext postdata, calcweb postdata)) (Prelude.tail tw)
 True  -> do
  posttw <- T.readFile noticetempconf
  tweet $ makeTweet (sendtext postdata) posttw
  -- print(makeTweet (sendtext.postdata) posttw) --for debug
  makeNotice (setPostData (sendtext postdata, calcweb postdata)) (Prelude.tail tw)

completePostData :: PostData -> [Text] -> Bool
completePostData postdata param = if Prelude.null param then True else
 if elem (Prelude.head param) ((Prelude.map fst.sendtext) postdata) then completePostData postdata (Prelude.tail param) else False
 
--calcWebPost :: PostData -> [Text] -> IO()
--calcWebPost postdata tw = do
-- nowpost <- Prelude.drop (Prelude.length(calcweb postdata)) <$> getDirectoryContents
-- case Prelude.null nowpost of
--  True  -> makeNotice (setPostData (sendtext postdata, calcweb postdata)) (Prelide.tail tw)
--  False -> makeNotice (setPostData (sendtext postdata, calcweb postdata)) (Prelide.tail tw)

 

makeTweet :: [(Text,Text)] -> Text -> Text
makeTweet postdata tw =  if Prelude.null postdata then tw else makeTweet (Prelude.tail postdata) (transTemp (Prelude.head postdata) tw (pack "")) 

transTemp :: (Text,Text) -> Text -> Text -> Text
transTemp (param,text) tw beftw = if Data.Text.null tw then beftw 
                                   else if Data.Text.take (Data.Text.length param + 2) tw == pack("\"" ++(unpack param)++"\"")
                                     then pack ((unpack beftw)++(unpack text)++((unpack.(Data.Text.drop (Data.Text.length param + 2))) tw))
                                   else transTemp (param,text) (Data.Text.tail tw) (pack ((unpack beftw) ++ [Data.Text.head tw]))  

userAdd :: PostData -> [Text] -> IO (PostData)
userAdd postdata tw = do
 user <- (getUser ((Data.Text.drop 9.Prelude.head) tw))
 case user of 
  Left err -> return postdata
  Right us -> do
   permituser <- Data.Text.lines<$>T.readFile permitconf
   case (not.elem ((gid_str.Prelude.head) us)) permituser of
    False -> makeNotice postdata (Prelude.tail tw)
    True -> do
     T.appendFile permitconf ((gid_str.Prelude.head) us) 
     makeNotice postdata (Prelude.tail tw)
