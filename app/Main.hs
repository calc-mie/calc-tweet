{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Lib
import Control.Concurrent
import Data.List
import qualified Data.Text.IO as T
import Data.Text

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
 case Data.Text.null puser of 
  True -> do 
   putStrLn "input userid"
   username <- T.getLine
   userid <- getUser username
   case userid of
    Left err -> putStrLn "error"
    Right ui -> T.appendFile permitconf ((gid_str.Prelude.head)ui)
  False -> putStrLn "start"

 -- main
 direct_message <- getGetDM
 case direct_message of
  Right dm -> do
   monitoring [] dm >> putStrLn "fin"

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


monitoring :: [(Text,Text)] -> GetEvents -> IO([(Text,Text)])
monitoring sendtext befdm= do
 threadDelay(61*1000*1000) -- 1minits
 directmessage <- getGetDM
 case directmessage of 
  Left err -> error err
  Right dm -> do
   if (getcreated_timestamp . Prelude.head . getevents) befdm == (getcreated_timestamp . Prelude.head . getevents) dm then  monitoring sendtext dm else do
    permissionuser <- Data.Text.lines <$> T.readFile permitconf-- idが許可された人かどうかを確認する.そうじゃなかったらmonitoring sendtextでloop
    case elemIndex ((getcreated_timestamp . Prelude.head . getevents) befdm) (Prelude.map (getcreated_timestamp) (getevents dm)) of 
     Nothing -> monitoring [] dm
     Just n -> do
      let puser = permissionIndexes ((Prelude.reverse.Prelude.map (getsender_id.getmessage_create)) ((Prelude.take n.getevents) dm)) permissionuser 0
      notices <- makeNotice ((Prelude.reverse.Prelude.map (gettext . getmessage_data . getmessage_create)) ((getPermitFromIndex puser.Prelude.take n.getevents) dm)) sendtext
      monitoring notices dm

permissionIndexes :: [Text] -> [Text] -> Int -> [Int]
permissionIndexes dm puser index = if Prelude.null dm then [] else 
 if elem (Prelude.head dm) puser then index:(permissionIndexes (Prelude.tail dm) puser (index + 1))
 else permissionIndexes (Prelude.tail dm) puser (index + 1)

getPermitFromIndex :: [Int] -> [GetMessageCreate] -> [GetMessageCreate]
getPermitFromIndex ind mcs = if Prelude.null ind then [] else ((mcs!!(Prelude.head ind)):(getPermitFromIndex (Prelude.tail ind) mcs))

makeNotice :: [Text] -> [(Text,Text)] -> IO([(Text,Text)]) 
makeNotice tw sendtext = do
 if Prelude.null tw then return sendtext else
  case ((unpack.Prelude.head.Prelude.head.Prelude.map Data.Text.words.Data.Text.lines.Prelude.head)tw) of
   "$notice" -> makeNotice (Prelude.tail tw) (("notice",(Data.Text.drop 8.Prelude.head) tw):sendtext)
   "$time"   -> makeNotice (Prelude.tail tw) (("time",(Data.Text.drop 6.Prelude.head) tw):sendtext)
   "$date"   -> makeNotice (Prelude.tail tw) (("date",(Data.Text.drop 6.Prelude.head) tw):sendtext)
   "$locale" -> makeNotice (Prelude.tail tw) (("locale",(Data.Text.drop 8.Prelude.head) tw):sendtext)
   "$clear"  -> makeNotice (Prelude.tail tw) []
   "$post"   -> do 
    posttw <- T.readFile templateconf
    tweet $ makeTweet sendtext posttw
    -- print(makeTweet sendtext posttw) --for debug
    makeNotice (Prelude.tail tw) []
   "$useradd"-> do
    user <- (getUser ((Data.Text.drop 9.Prelude.head) tw))
    case user of 
     Left err -> return sendtext
     Right us -> do
      permituser <- Data.Text.lines<$>T.readFile permitconf
      case (not.elem ((gid_str.Prelude.head) us)) permituser of
       True -> T.appendFile permitconf ((gid_str.Prelude.head) us) 
       False -> putStrLn "error:this user has been adding"
      makeNotice (Prelude.tail tw) sendtext
   otherwise -> makeNotice (Prelude.tail tw) sendtext
   
makeTweet :: [(Text,Text)] -> Text -> Text
makeTweet sendtext tw =  if Prelude.null sendtext then tw else makeTweet (Prelude.tail sendtext) (transTemp tw (Prelude.head sendtext) []) 

transTemp :: Text -> (Text,Text) -> Text -> Text
transTemp tw (param,text) beftw = if Data.Text.null tw then beftw 
                                   else if Data.Text.take (Data.Text.length param + 2) tw == pack("\"" ++(unpack param)++"\"")
                                     then pack ((unpack beftw)++(unpack text)++((unpack.(Data.Text.drop (Data.Text.length param + 2))) tw))
                                   else transTemp (Data.Text.tail tw) (param,text) (pack ((unpack beftw) ++ [Data.Text.head tw]))
 
