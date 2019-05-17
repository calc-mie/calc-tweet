module Lib ( calcwebdir
           , srvcalcdir
           , PostData (..)
           , monitoring
           , setPostData)where

import TwitterAPI
import SlackAPI
import Control.Exception
import Control.Concurrent
import Data.List
import qualified Data.Text.IO as T
import Data.Text
import Data.Time
import System.Directory

data PostData = PostData { sendtext :: [SendTL]
                         , calcweb :: [String] -- calc's post before
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

calcwebdir = "/home/share/posts/posts-available/"
srvcalcdir = "/srv/calc-web/posts"

monitoring :: PostData -> GetEvents -> IO PostData
monitoring pd befdm= do
 threadDelay(61*1000*1000) -- 1minits
 postdata <- rtCheck pd-- monitoring retweeting
 -- monitoring direct message
 directmessage <- getGetDM
 case directmessage of 
  Left err -> error err
  Right dm -> if (getcreated_timestamp . Prelude.head . getevents) befdm == (getcreated_timestamp . Prelude.head . getevents) dm 
               then monitoring postdata dm 
              else do
               permissionuser <- Data.Text.lines <$> T.readFile permitconf
               case elemIndex ((getcreated_timestamp . Prelude.head . getevents) befdm) (Prelude.map getcreated_timestamp (getevents dm)) of 
                Nothing -> monitoring postdata dm
                Just n -> do
                 let puser = permissionIndexes ((Prelude.reverse.Prelude.map (getsender_id.getmessage_create)) ((Prelude.take n.getevents) dm)) permissionuser 0
--                 print ((Prelude.reverse.getPermitFromIndex puser.Prelude.take n.getevents) dm)
                 notices <- makeNotice postdata ((Prelude.reverse.getPermitFromIndex puser.Prelude.take n.getevents) dm)
                 monitoring notices dm 

permissionIndexes :: [Text] -> [Text] -> Int -> [Int]
permissionIndexes dm puser index 
 | Prelude.null dm              = []  
 | Prelude.head dm `elem` puser = index:permissionIndexes (Prelude.tail dm) puser (index + 1)
 | otherwise                    = permissionIndexes (Prelude.tail dm) puser (index + 1)

getPermitFromIndex :: [Int] -> [GetMessageCreate] -> [GetMessageCreate]
getPermitFromIndex ind mcs = if Prelude.null ind then [] else (mcs!!Prelude.head ind):getPermitFromIndex (Prelude.tail ind) mcs

makeNotice :: PostData -> [GetMessageCreate] -> IO PostData 
makeNotice postdata tw 
 | Prelude.null tw        = return postdata 
 | Prelude.length tw > 20 = return (setPostData([], calcweb postdata, schedule postdata)) 
 | otherwise              = 
  case (unpack.Prelude.head.Prelude.head.Prelude.map Data.Text.words.Data.Text.lines.gettext.getmessage_data.getmessage_create.Prelude.head) tw of
   "$notice"        -> nextMakeNotice postdata tw "notice"
   "$time"          -> nextMakeNotice postdata tw "time"
   "$date"          -> nextMakeNotice postdata tw "date"
   "$locale"        -> nextMakeNotice postdata tw "locale"
   "$clear"         -> makeNotice (setPostData ([], calcweb postdata, schedule postdata)) (Prelude.tail tw)
   "$post"          -> postTweet postdata tw
   "$post-calc-web" -> calcWebPost postdata tw
   "$useradd"       -> userAdd postdata tw
   _                -> makeNotice postdata (Prelude.tail tw)

texttwopert = Data.Text.unwords.Prelude.tail.Data.Text.words.gettext.getmessage_data.getmessage_create.Prelude.head
textothpert = Data.Text.unwords.Prelude.tail.Prelude.tail.Data.Text.words.gettext.getmessage_data.getmessage_create.Prelude.head
idpert = getsender_id.getmessage_create.Prelude.head
numpert = read.unpack.Data.Text.tail.Prelude.head.Prelude.tail.Data.Text.words.gettext.getmessage_data.getmessage_create.Prelude.head

nextMakeNotice :: PostData -> [GetMessageCreate] -> String -> IO PostData
nextMakeNotice pd tw par = 
 case (Prelude.length.Data.Text.words.gettext.getmessage_data.getmessage_create.Prelude.head) tw of
  2 -> makeNotice (setPostData (setSendTL (pack par, texttwopert tw, idpert tw, 1):sendtext pd, calcweb pd, schedule pd)) (Prelude.tail tw)
  _ -> makeNotice ( if (Data.Text.head.Prelude.head.Prelude.tail.Data.Text.words.gettext.getmessage_data.getmessage_create.Prelude.head) tw == '-' then
                     setPostData (setSendTL (pack par, textothpert tw, idpert tw, numpert tw):sendtext pd, calcweb pd, schedule pd)
                    else 
                     setPostData (setSendTL (pack par, texttwopert tw, idpert tw, 1):sendtext pd, calcweb pd, schedule pd) )
                  (Prelude.tail tw)

setSendTL :: (Text,Text,Text,Int) -> SendTL
setSendTL (param, sent, sender, n) = SendTL {parameter = param, sentence = sent, sender_id = sender, num=n}

setPostData :: ([SendTL],[String],[(Text,ZonedTime)]) -> PostData
setPostData (sendtx, web, sche) = PostData { sendtext = sendtx, calcweb = web , schedule = sche}

postTweet :: PostData -> [GetMessageCreate] -> IO PostData
postTweet postdata tw = do 
 let ntdata = createNoticeData (Prelude.filter (((getsender_id.getmessage_create.Prelude.head) tw ==).sender_id) (sendtext postdata)) 
                               NoticeData{notice = pack "", date = [], time = [], locale =[]}
 if (Data.Text.null.notice) ntdata then makeNotice postdata (Prelude.tail tw)
 else (do
   posttw <- T.readFile noticetempconf
   let posttx = makeTweet ntdata 1 ((Prelude.maximum.Prelude.map (Prelude.maximum.Prelude.map snd.((pack "null",0):)))[date ntdata, time ntdata, locale ntdata]) 
                                                                 (Data.Text.append posttw (Data.Text.append (notice ntdata) (pack "\n")))
   response <- tweet posttx
   postSlack posttx
   case response of
    Left err ->  makeNotice postdata (Prelude.tail tw)
    Right re -> do
     rttime <- setNoticeTime postdata ntdata (id_str re)
-- debug
--   let response = PostTL {id_str = pack "1129405960840028160"}
--   print posttx
--   rttime <- setNoticeTime postdata ntdata (id_str response)
     makeNotice (setPostData (Prelude.filter (((getsender_id.getmessage_create.Prelude.head) tw /=).sender_id) (sendtext postdata) ,calcweb postdata ,rttime))
                (Prelude.tail tw) )



calcWebPost :: PostData -> [GetMessageCreate] -> IO PostData
calcWebPost postdata tw = do
 nowpost <- getDirectoryContents srvcalcdir
 let newarticle = Prelude.filter (\x->x `notElem` (calcweb postdata)) nowpost
 if Prelude.null newarticle then makeNotice (setPostData (sendtext postdata, calcweb postdata, schedule postdata)) (Prelude.tail tw)
 else loop newarticle nowpost
  where
   loop :: [String] -> [String] -> IO PostData
   loop na np = if Prelude.null na then makeNotice (setPostData (sendtext postdata, np, schedule postdata)) (Prelude.tail tw)
                else ( do
                 article <- Data.Text.lines<$>T.readFile (calcwebdir ++ ((Prelude.takeWhile (/= '.')).Prelude.head) na ++ ".md")
                 let title  = Data.Text.drop 7 (article!!1)
                     author = (article!!2)
                     webtx  =  pack $   unpack author ++ "\n" 
                                     ++ unpack title ++ "について書きました。\n url: https://calc.mie.jp/posts/" 
                                     ++ ((Prelude.takeWhile (/= '.')).Prelude.head) na
                 tweet webtx
                 postSlack webtx
                 loop (Prelude.tail na) np )
  
createNoticeData :: [SendTL] -> NoticeData -> NoticeData
createNoticeData sendtl ntdata = if Prelude.null sendtl then ntdata else createNoticeData (Prelude.tail sendtl) (
 case (unpack.parameter.Prelude.head) sendtl of
  "notice" -> NoticeData{notice = (sentence.Prelude.head) sendtl, date = date ntdata ,time = time ntdata,locale = locale ntdata}
  "date"   -> NoticeData{notice = notice ntdata, date = addND (Prelude.head sendtl) (date ntdata), time = time ntdata, locale = locale ntdata}
  "time"   -> NoticeData{notice = notice ntdata, date = date ntdata, time = addND (Prelude.head sendtl) (time ntdata), locale = locale ntdata}
  "locale" -> NoticeData{notice = notice ntdata, date = date ntdata, time = time ntdata, locale = addND (Prelude.head sendtl) (locale ntdata)}
 )
 
addND :: SendTL -> [(Text,Int)] -> [(Text,Int)]
addND ad list = if num ad `elem` Prelude.map snd list then list else (sentence ad,num ad):list

makeTweet :: NoticeData -> Int -> Int -> Text -> Text
makeTweet ntdata n mx tw = if n>mx then tw else 
 makeTweet ntdata (n+1) mx (if n `elem` Prelude.concatMap (Prelude.map snd) [date ntdata, time ntdata, locale ntdata] then
                             Data.Text.append tw (
                             Data.Text.append (elemText n (date ntdata)) (
                             Data.Text.append (elemText n (time ntdata)) (
                             Data.Text.append (if (Data.Text.null.elemText n) (locale ntdata) then pack "" 
                                                else Data.Text.append (pack "＠.") (elemText n (locale ntdata))) (pack "\n"))))  -- createSchedule
                             else makeTweet ntdata (n+1) mx tw)

elemText :: Int -> [(Text, Int)] -> Text
elemText n text = if (n `elem` Prelude.map snd text) || (not.Prelude.any ((==n).snd)) text then pack "" 
                        else Data.Text.append ((fst.Prelude.head.Prelude.filter ((==n).snd)) text) (pack " ")

userAdd :: PostData -> [GetMessageCreate] -> IO PostData
userAdd postdata tw = do
 user <- getUser ((Data.Text.drop 9.gettext.getmessage_data.getmessage_create.Prelude.head) tw)
 case user of 
  Left err -> return postdata
  Right us -> do
   permituser <- Data.Text.lines<$>T.readFile permitconf
   if (gid_str.Prelude.head) us  `elem` permituser then 
    (do
     T.appendFile permitconf ((gid_str.Prelude.head) us) 
     makeNotice postdata (Prelude.tail tw) )
   else makeNotice postdata (Prelude.tail tw) 

setNoticeTime :: PostData -> NoticeData -> Text -> IO [(Text,ZonedTime)]
setNoticeTime pdt ndt res = loop 1 ((Prelude.maximum.Prelude.map (Prelude.maximum.Prelude.map snd.((pack "null",0):)))[date ndt, time ndt]) [date ndt, time ndt] pdt
 where 
   loop :: Int -> Int -> [[(Text,Int)]] -> PostData -> IO[(Text,ZonedTime)]
   loop n mx [dat, tim] pdt
    | n>mx = (return.schedule) pdt 
    | Prelude.all (elem n.Prelude.map snd) [dat, tim] = do
       ctz     <- getCurrentTimeZone
       (ft,lt) <- getNum ':' ((fst.Prelude.head.Prelude.filter ((==n).snd)) tim) (25,61)
       (fd,ld) <- getNum '/' ((fst.Prelude.head.Prelude.filter ((==n).snd)) dat) (13,32)
       case makeTimeOfDayValid ft lt 0 of
        Nothing -> loop (n+1) mx [dat,tim] pdt 
        Just t  -> case fromGregorianValid 2019 fd ld of
                    Nothing -> loop (n+1) mx [dat,tim] pdt
                    Just d  -> do
                     next <- loop (n+1) mx [dat,tim] pdt
                     return ((res,ZonedTime { zonedTimeToLocalTime = LocalTime { localDay = d
                                                                               , localTimeOfDay = t},
                                              zonedTimeZone = ctz}):next) 
    | otherwise =  loop (n+1) mx [dat, tim] pdt

getNum :: Char -> Text -> (Int,Int) -> IO(Int,Int)
getNum c text (f,l) = if Data.Text.null text then return (f,l) else case (elemIndex c.unpack.Data.Text.filter (\x->x `elem` (c:['0'..'9']))) text of
 Nothing -> return (f,l)
 Just n  -> catch(
             (return.(\(a,b)->((read.unpack) a,(read.unpack.Data.Text.take 2.Data.Text.tail) b)).Data.Text.splitAt n.Data.Text.filter (\x->x `elem` (c:['0'..'9']))) text)
             $ \(SomeException e) -> return (f,l)

rtCheck :: PostData -> IO PostData
rtCheck postdata = do
 now <- zonedTimeToUTC<$>getZonedTime
 let rtlist = Prelude.filter ((<60*60).((`diffUTCTime` now).zonedTimeToUTC.snd)) (schedule postdata)
 case Prelude.null rtlist of
  True  -> return postdata 
  False -> loop rtlist postdata
   where 
    loop rtl pdt = if Prelude.null rtl then
                    return PostData{ sendtext = sendtext pdt
                                   , calcweb  = calcweb pdt 
                                   , schedule = Prelude.filter ((>60*60).((`diffUTCTime` now).zonedTimeToUTC.snd)) (schedule pdt)}
                   else  
                    (do
                      postRT ((fst.Prelude.head) rtl) 
                      -- print ((fst.Prelude.head) rtl) -- for debug
                      loop (Prelude.tail rtl) pdt )
