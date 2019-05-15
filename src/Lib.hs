module Lib ( PostData (..)
           , monitoring
           , setPostData)where

import TwitterAPI
import Control.Exception
import Control.Concurrent
import Data.List
import qualified Data.Text.IO as T
import Data.Text
import Data.Time
--import System.Directory

data PostData = PostData { sendtext :: [SendTL]
                         , calcweb :: String -- calc's post before
                         , schedule :: [(Text, ZonedTime)] -- post text (twid, time)
                         } deriving (Show)

data SendTL = SendTL { parameter :: Text
                     , sentence :: Text
                     , sender_id :: Text
                     , num :: Int
                     } deriving (Show)

data NoticeData = NoticeData { notice :: Text
                             , date :: [(Text,Int)]
                             , time :: [(Text,Int)]
                             , locale :: [(Text,Int)]
                             } deriving (Show)

monitoring :: PostData -> GetEvents -> IO(PostData)
monitoring pd befdm= do
 threadDelay(61*1000*1000) -- 1minits
 postdata <- rtCheck pd-- monitoring retweeting
 -- monitoring direct message
 directmessage <- getGetDM
 case directmessage of 
  Left err -> error err
  Right dm -> do
   if (getcreated_timestamp . Prelude.head . getevents) befdm == (getcreated_timestamp . Prelude.head . getevents) dm then  monitoring postdata dm else do
    permissionuser <- Data.Text.lines <$> T.readFile permitconf
    case elemIndex ((getcreated_timestamp . Prelude.head . getevents) befdm) (Prelude.map (getcreated_timestamp) (getevents dm)) of 
     Nothing -> monitoring postdata dm
     Just n -> do
      let puser = permissionIndexes ((Prelude.reverse.Prelude.map (getsender_id.getmessage_create)) ((Prelude.take n.getevents) dm)) permissionuser 0
--      print ((Prelude.reverse.getPermitFromIndex puser.Prelude.take n.getevents) dm)
      notices <- makeNotice postdata ((Prelude.reverse.getPermitFromIndex puser.Prelude.take n.getevents) dm)
      monitoring notices dm 

permissionIndexes :: [Text] -> [Text] -> Int -> [Int]
permissionIndexes dm puser index = if Prelude.null dm then [] else 
 if elem (Prelude.head dm) puser then index:(permissionIndexes (Prelude.tail dm) puser (index + 1))
 else permissionIndexes (Prelude.tail dm) puser (index + 1)

getPermitFromIndex :: [Int] -> [GetMessageCreate] -> [GetMessageCreate]
getPermitFromIndex ind mcs = if Prelude.null ind then [] else ((mcs!!(Prelude.head ind)):(getPermitFromIndex (Prelude.tail ind) mcs))

makeNotice :: PostData -> [GetMessageCreate] -> IO(PostData) 
makeNotice postdata tw = do
 if Prelude.null tw then return postdata else if Prelude.length tw > 20 then return (setPostData([],(calcweb postdata),(schedule postdata))) else
  case ((unpack.Prelude.head.Prelude.head.Prelude.map Data.Text.words.Data.Text.lines.gettext.getmessage_data.getmessage_create.Prelude.head)tw) of
   "$notice"        -> nextMakeNotice postdata tw "notice"
   "$time"          -> nextMakeNotice postdata tw "time"
   "$date"          -> nextMakeNotice postdata tw "date"
   "$locale"        -> nextMakeNotice postdata tw "locale"
   "$clear"         -> makeNotice (setPostData([],(calcweb postdata),(schedule postdata))) (Prelude.tail tw)
   "$post"          -> postTweet postdata tw
--   "$post-calc-web" -> calcWebPost postdata tw
   "$useradd"       -> userAdd postdata tw
   otherwise        -> makeNotice postdata (Prelude.tail tw)

texttwopert = Data.Text.unwords.Prelude.tail.Data.Text.words.gettext.getmessage_data.getmessage_create.Prelude.head
textothpert = Data.Text.unwords.Prelude.tail.Prelude.tail.Data.Text.words.gettext.getmessage_data.getmessage_create.Prelude.head
idpert = getsender_id.getmessage_create.Prelude.head
numpert = read.unpack.Data.Text.tail.Prelude.head.Prelude.tail.Data.Text.words.gettext.getmessage_data.getmessage_create.Prelude.head

nextMakeNotice :: PostData -> [GetMessageCreate] -> String -> IO(PostData)
nextMakeNotice pd tw par = 
 case (Prelude.length.Data.Text.words.gettext.getmessage_data.getmessage_create.Prelude.head) tw of
  2         -> makeNotice (setPostData(((setSendTL (pack par, texttwopert tw, idpert tw, 1)):(sendtext pd), (calcweb pd), schedule pd))) (Prelude.tail tw)
  otherwise -> case ((Data.Text.head.Prelude.head.Prelude.tail.Data.Text.words.gettext.getmessage_data.getmessage_create.Prelude.head) tw) == '-' of
   True  -> makeNotice (setPostData(((setSendTL (pack par, textothpert tw, idpert tw, numpert tw)):(sendtext pd), (calcweb pd), schedule pd))) (Prelude.tail tw)
   False -> makeNotice (setPostData(((setSendTL (pack par, texttwopert tw, idpert tw, 1)):(sendtext pd), (calcweb pd), schedule pd))) (Prelude.tail tw)

setSendTL :: (Text,Text,Text,Int) -> SendTL
setSendTL (param, sent, sender, n) = SendTL {parameter = param, sentence = sent, sender_id = sender, num=n}

setPostData :: ([SendTL],String,[(Text,ZonedTime)]) -> PostData
setPostData (sendtx, web, sche) = PostData { sendtext = sendtx, calcweb = web , schedule = sche}

postTweet :: PostData -> [GetMessageCreate] -> IO (PostData)
postTweet postdata tw = do 
 let ntdata = createNoticeData (Prelude.filter (((==) ((getsender_id.getmessage_create.Prelude.head)tw)).sender_id) (sendtext postdata)) 
                               NoticeData{notice = pack "", date = [], time = [], locale =[]}
 case (Data.Text.null.notice) ntdata of 
  True  -> makeNotice postdata (Prelude.tail tw)
  False -> do
   posttw <- T.readFile noticetempconf
   response <- tweet $ makeTweet ntdata 1 ((Prelude.maximum.Prelude.map (Prelude.maximum.Prelude.map snd.((pack "null",0):)))[date ntdata, time ntdata, locale ntdata]) 
                                          (Data.Text.append posttw (Data.Text.append (notice ntdata) (pack "\n")))

--   putStrLn "post"
--   (print.sendtext) postdata
--   print ntdata
--   print $ makeTweet ntdata 1 ((Prelude.maximum.Prelude.map (Prelude.maximum.Prelude.map snd))[date ntdata, time ntdata, locale ntdata]) 
--                              (Data.Text.append posttw (Data.Text.append (notice ntdata) (pack "\n"))) --for debug

--   let response = Right (pack "po")
   case response of
    Left err ->  makeNotice postdata (Prelude.tail tw)
    Right re -> do
     rttime <- setNoticeTime postdata ntdata (id_str re)
     makeNotice (setPostData ((Prelude.filter (((/=) ((getsender_id.getmessage_create.Prelude.head)tw)).sender_id) (sendtext postdata)) ,calcweb postdata ,rttime))
                (Prelude.tail tw)

--calcWebPost :: PostData -> [Text] -> IO()
--calcWebPost postdata tw = do
-- nowpost <- Prelude.drop (Prelude.length(calcweb postdata)) <$> getDirectoryContents
-- case Prelude.null nowpost of
--  True  -> makeNotice (setPostData (sendtext postdata, calcweb postdata)) (Prelide.tail tw)
--  False -> makeNotice (setPostData (sendtext postdata, calcweb postdata)) (Prelide.tail tw)

createNoticeData :: [SendTL] -> NoticeData -> NoticeData
createNoticeData sendtl ntdata = if Prelude.null sendtl then ntdata else createNoticeData (Prelude.tail sendtl) (
 case (unpack.parameter.Prelude.head) sendtl of
  "notice" -> NoticeData{notice = (sentence.Prelude.head) sendtl, date = date ntdata ,time = time ntdata,locale = locale ntdata}
  "date"   -> NoticeData{notice = notice ntdata, date = addND ((Prelude.head) sendtl) (date ntdata), time = time ntdata, locale = locale ntdata}
  "time"   -> NoticeData{notice = notice ntdata, date = date ntdata, time = addND ((Prelude.head) sendtl) (time ntdata), locale = locale ntdata}
  "locale" -> NoticeData{notice = notice ntdata, date = date ntdata, time = time ntdata, locale = addND ((Prelude.head) sendtl) (locale ntdata)}
 )
 
addND :: SendTL -> [(Text,Int)] -> [(Text,Int)]
addND ad list = case elem (num ad) (Prelude.map snd list) of
 True  -> list
 False -> (sentence ad,num ad):list

makeTweet :: NoticeData -> Int -> Int -> Text -> Text
makeTweet ntdata n mx tw = if n>mx then tw else case elem n ((Prelude.concat.Prelude.map (Prelude.map snd)) [date ntdata, time ntdata, locale ntdata]) of
  False -> makeTweet ntdata (n+1) mx tw 
  True  -> makeTweet ntdata (n+1) mx (Data.Text.append tw (
                                      Data.Text.append (elemText n (date ntdata)) (
                                      Data.Text.append (elemText n (time ntdata)) (
                                      Data.Text.append (if (Data.Text.null.elemText n) (locale ntdata) then pack "" else  
                                       Data.Text.append (pack "＠.") (elemText n (locale ntdata))) (pack "\n")))))

elemText :: Int -> [(Text, Int)] -> Text
elemText n text = case elem n (Prelude.map snd text) of 
 False -> pack ""
 True  -> if (Prelude.null.Prelude.filter ((==n).snd)) text then pack "" else Data.Text.append ((fst.Prelude.head.Prelude.filter ((==n).snd)) text) (pack " ")

userAdd :: PostData -> [GetMessageCreate] -> IO (PostData)
userAdd postdata tw = do
 user <- (getUser ((Data.Text.drop 9.gettext.getmessage_data.getmessage_create.Prelude.head) tw))
 case user of 
  Left err -> return postdata
  Right us -> do
   permituser <- Data.Text.lines<$>T.readFile permitconf
   case (not.elem ((gid_str.Prelude.head) us)) permituser of
    False -> makeNotice postdata (Prelude.tail tw)
    True -> do
     T.appendFile permitconf ((gid_str.Prelude.head) us) 
     makeNotice postdata (Prelude.tail tw)

setNoticeTime :: PostData -> NoticeData -> Text -> IO ([(Text,ZonedTime)])
setNoticeTime pdt ndt res = loop 1 ((Prelude.maximum.Prelude.map (Prelude.maximum.Prelude.map snd.((pack "null",0):)))[date ndt, time ndt]) [date ndt, time ndt] pdt res
 where 
   loop :: Int -> Int -> [[(Text,Int)]] -> PostData -> Text -> IO[(Text,ZonedTime)]
   loop n mx [dat, tim] pdt res = if n>mx then (return.schedule) pdt else case (and.Prelude.map (elem n.(Prelude.map snd))) [dat, tim] of
    False -> loop (n+1) mx [dat, tim] pdt res
    True -> do
     ctz <- getCurrentTimeZone
     (ft,lt) <- getNum ':' ((fst.Prelude.head.Prelude.filter ((==n).snd)) tim) (25,61)
     (fd,ld) <- getNum '/' ((fst.Prelude.head.Prelude.filter ((==n).snd)) dat) (13,32)
     case makeTimeOfDayValid ft lt 0 of
      Nothing -> loop (n+1) mx [dat,tim] pdt res
      Just t  -> case fromGregorianValid 2019 fd ld of
       Nothing -> loop (n+1) mx [dat,tim] pdt res
       Just d  -> do
        next <- loop (n+1) mx [dat,tim] pdt res
        return ((res,ZonedTime { zonedTimeToLocalTime = LocalTime { localDay = d, localTimeOfDay = t}, zonedTimeZone = ctz}):next)

getNum :: Char -> Text -> (Int,Int) -> IO((Int,Int))
getNum c text (f,l) = if Data.Text.null text then return (f,l) else case (elemIndex c.unpack.(Data.Text.filter (\x->elem x (c:['0'..'9'])))) text of
 Nothing -> return (f,l)
 Just n  -> catch((return.(\(a,b)->((read.unpack) a,(read.unpack.Data.Text.take 2.Data.Text.tail) b)).Data.Text.splitAt n.Data.Text.filter (\x->elem x (c:['0'..'9']))) text)
             $ \(SomeException e) -> return (f,l)

rtCheck :: PostData -> IO (PostData)
rtCheck postdata = do
 now <- zonedTimeToUTC<$>getZonedTime
 let rtlist = (Prelude.filter ((<60*60).((`diffUTCTime` now).zonedTimeToUTC.snd))) (schedule postdata)
 case Prelude.null rtlist of
  True  -> return postdata 
  False -> loop rtlist postdata
   where 
    loop rtl pdt = case Prelude.null rtl of
     True  -> return PostData{ sendtext = sendtext pdt, calcweb = calcweb pdt 
                             , schedule = (Prelude.filter ((>60*60).((`diffUTCTime` now).zonedTimeToUTC.snd))) (schedule pdt)}
     False -> do
      resp<-postRT ((fst.Prelude.head) rtl) 
      loop (Prelude.tail rtl) pdt

