module Lib where

import SlackAPI
import TwitterAPI
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
                         , noon :: Bool
                         } deriving (Show)

data SendTL = SendTL { parameter :: Text
                     , sentence :: Text
                     , sender_id :: Text
                     , num :: Int
                     } deriving (Show)

data NoticeData = NoticeData { notice :: [Text]
                             , date :: [(Text,Int)]
                             , time :: [(Text,Int)]
                             , locale :: [(Text,Int)]
                             } deriving (Show)

data Week = Monday
          | Tuesday
          | Wednesday
          | Thursday
          | Friday
          | Saturday
          | Sunday
          deriving (Show, Enum, Eq)

data PostType = TL
              | DM
              deriving (Eq)

calcwebdir = "/home/share/posts/posts-available/"
srvcalcdir = "/srv/calc-web/posts"
reminddir = "/usr/local/calc-tweet/reminder/"


permissionIndexes :: [Text] -> [User] -> Int -> [Int]
permissionIndexes dm puser index 
 | Prelude.null dm                                    = []  
 | Prelude.head dm `elem` (Prelude.map gid_str puser) = index:permissionIndexes (Prelude.tail dm) puser (index + 1)
 | otherwise                                          = permissionIndexes (Prelude.tail dm) puser (index + 1)

getPermitFromIndex :: [Int] -> [GetMessageCreate] -> [GetMessageCreate]
getPermitFromIndex ind mcs = if Prelude.null ind then [] else (mcs!!Prelude.head ind):getPermitFromIndex (Prelude.tail ind) mcs

dmTotext = gettext.getmessage_data.getmessage_create.Prelude.head
textTolistlisttext = Prelude.map Data.Text.words.Data.Text.lines
listlisttextTotext = Data.Text.init.Data.Text.unlines.Prelude.map Data.Text.unwords
texttwopert = listlisttextTotext.(\n->(Prelude.tail.Prelude.head)n:(Prelude.tail n)).textTolistlisttext.dmTotext
textothpert = listlisttextTotext.(\n->(Prelude.tail.Prelude.tail.Prelude.head)n:(Prelude.tail n)).textTolistlisttext.dmTotext
idpert = getsender_id.getmessage_create.Prelude.head
numpert = read.unpack.Data.Text.tail.Prelude.head.Prelude.tail.Data.Text.words.dmTotext

nextMakeNotice :: PostData -> [GetMessageCreate] -> String -> PostData
nextMakeNotice pd tw par = 
 case (Prelude.length.Data.Text.words.gettext.getmessage_data.getmessage_create.Prelude.head) tw of
  2 -> (setPostData (setSendTL (pack par, texttwopert tw, idpert tw, 1):sendtext pd, calcweb pd, schedule pd, noon pd))
  _ -> ( if (Data.Text.head.Prelude.head.Prelude.tail.Data.Text.words.gettext.getmessage_data.getmessage_create.Prelude.head) tw == '-' then
                     setPostData (setSendTL (pack par, textothpert tw, idpert tw, numpert tw):sendtext pd, calcweb pd, schedule pd, noon pd)
                    else 
                     setPostData (setSendTL (pack par, texttwopert tw, idpert tw, 1):sendtext pd, calcweb pd, schedule pd, noon pd) )

setSendTL :: (Text,Text,Text,Int) -> SendTL
setSendTL (param, sent, sender, n) = SendTL {parameter = param, sentence = sent, sender_id = sender, num=n}

setPostData :: ([SendTL],[String],[(Text,ZonedTime)], Bool) -> PostData
setPostData (sendtx, web, sche, non) = PostData { sendtext = sendtx, calcweb = web , schedule = sche, noon = non}

postTweet :: PostData -> [GetMessageCreate] -> (Text -> PostData -> [GetMessageCreate] -> IO Text) -> IO PostData
postTweet postdata tw ptfunc= do 
 let ntdata = createNoticeData ((Prelude.reverse.Prelude.filter (((getsender_id.getmessage_create.Prelude.head) tw ==).sender_id)) (sendtext postdata))
                               NoticeData{notice = [], date = [], time = [], locale =[]}
 if (Prelude.null.notice) ntdata then return postdata
 else (do
  posttw <- T.readFile noticetempconf
  let posttx = makeTweet ntdata 1 ((Prelude.maximum.Prelude.map (Prelude.maximum.Prelude.map snd.((pack "null",0):)))[date ntdata, time ntdata, locale ntdata]) 
                                                                (Data.Text.append posttw (Data.Text.append ((Prelude.head.notice) ntdata) (pack "\n")))
  postid_str <- ptfunc posttx postdata tw
  if Data.Text.null postid_str then return postdata
  else setNoticeTime  postdata ntdata postid_str >>=
   (\r->return postdata { sendtext = Prelude.filter (((getsender_id.getmessage_create.Prelude.head) tw /=).sender_id) (sendtext postdata) 
                        , schedule = r}) )

calcWebPost :: PostData -> [GetMessageCreate] -> (Text -> PostData -> [GetMessageCreate] -> IO Text) -> IO PostData
calcWebPost postdata tw ptfunc= do
 nowpost <- getDirectoryContents srvcalcdir
 let newarticle = Prelude.filter (\x->x `notElem` (calcweb postdata)) nowpost
 if Prelude.null newarticle then return postdata
 else loop newarticle nowpost
  where
   loop :: [String] -> [String] -> IO PostData
   loop na np = if Prelude.null na then return postdata { calcweb = np }
                else ( do
                 article <- Data.Text.lines<$>T.readFile (calcwebdir ++ ((Prelude.takeWhile (/= '.')).Prelude.head) na ++ ".md")
                 let title  = Data.Text.drop 7 (article!!1)
                     author = (article!!2)
                     webtx  =  pack $   unpack author ++ "\n" 
                                     ++ unpack title ++ "について書きました。\n url: https://calc.mie.jp/posts/" 
                                     ++ Prelude.head na
                 ptfunc webtx postdata tw
                 loop (Prelude.tail na) np )
  
createNoticeData :: [SendTL] -> NoticeData -> NoticeData
createNoticeData sendtl ntdata = if Prelude.null sendtl then ntdata else createNoticeData (Prelude.tail sendtl) (
 case (unpack.parameter.Prelude.head) sendtl of
  "notice" -> NoticeData{notice = (sentence.Prelude.head) sendtl:notice ntdata, date = date ntdata ,time = time ntdata,locale = locale ntdata}
  "date"   -> NoticeData{notice = notice ntdata, date = addND (Prelude.head sendtl) (date ntdata), time = time ntdata, locale = locale ntdata}
  "time"   -> NoticeData{notice = notice ntdata, date = date ntdata, time = addND (Prelude.head sendtl) (time ntdata), locale = locale ntdata}
  "locale" -> NoticeData{notice = notice ntdata, date = date ntdata, time = time ntdata, locale = addND (Prelude.head sendtl) (locale ntdata)}
 )
 
addND :: SendTL -> [(Text,Int)] -> [(Text,Int)]
addND ad list = (sentence ad,num ad):list

makeTweet :: NoticeData -> Int -> Int -> Text -> Text
makeTweet ntdata n mx tw = if n>mx then tw else 
 makeTweet ntdata (n+1) mx (if n `elem` Prelude.concatMap (Prelude.map snd) [date ntdata, time ntdata, locale ntdata] then
                             Data.Text.append tw (
                             Data.Text.append (elemText n (date ntdata)) (
                             Data.Text.append (elemText n (time ntdata)) (
                             Data.Text.append (if (Data.Text.null.elemText n) (locale ntdata) then pack "" 
                                                else Data.Text.append (pack "＠.") (elemText n (locale ntdata))) (pack "\n"))))  -- createSchedule
                             else tw)

elemText :: Int -> [(Text, Int)] -> Text
elemText n text = if (n `notElem` Prelude.map snd text) then pack "" 
                        else Data.Text.append ((fst.Prelude.head.Prelude.filter ((==n).snd)) text) (pack " ")

userAdd :: PostData -> [GetMessageCreate] -> IO PostData
userAdd postdata tw = do
 let user = ((Data.Text.drop 9.gettext.getmessage_data.getmessage_create.Prelude.head) tw)
 permituser <- Data.Text.lines<$>T.readFile permitconf
 if user `notElem` permituser then (T.appendFile permitconf user >> return postdata)
 else return postdata 

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
       year <- (\(y,m,d)->if fd < m then y+1 else y).toGregorian.localDay.zonedTimeToLocalTime<$>getZonedTime
       case makeTimeOfDayValid ft lt 0 of
        Nothing -> loop (n+1) mx [dat,tim] pdt 
        Just t  -> case fromGregorianValid year fd ld of
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
 let rtlist = Prelude.filter ((<=15*60).((`diffUTCTime` now).zonedTimeToUTC.snd)) (schedule postdata)
 case Prelude.null rtlist of
  True  -> return postdata 
  False -> loop rtlist postdata
   where 
    loop rtl pdt = if Prelude.null rtl then
                    return (setPostData ( sendtext pdt
                                        , calcweb pdt 
                                        , Prelude.filter ((>15*60).((`diffUTCTime` now).zonedTimeToUTC.snd)) (schedule pdt)
                                        , noon pdt))
                   else  
                    (do
                      postRT ((fst.Prelude.head) rtl) 
                      loop (Prelude.tail rtl) pdt )

remindCheck :: (Text -> PostData -> [GetMessageCreate] -> IO Text) ->  PostData -> IO PostData
remindCheck ptfunc postdata = do
 today <- zonedTimeToLocalTime<$>getZonedTime
 case divMod ((todHour.localTimeOfDay) today) 14 of
  (0,_)  -> return postdata { noon = False }
  (1,0)  -> getDirectoryContents reminddir >>= (\fs -> loop postdata today ptfunc (Prelude.map (reminddir++) fs))
  (1,_)  -> return postdata { noon = True }
  where
   loop :: PostData -> LocalTime -> (Text -> PostData -> [GetMessageCreate] -> IO Text) -> [FilePath] -> IO PostData
   loop pd td ptfunc file = if Prelude.null file || noon pd then return pd { noon = True }
    else (doesFileExist.Prelude.head) file >>= 
     (\check -> if not check then loop pd td ptfunc (Prelude.tail file)
     else ( do
      (getWeek, time, text)<-(\f->((read.unpack.Prelude.head) f
                                  ,(Prelude.head.Prelude.tail) f
                                  ,(Data.Text.unlines.Prelude.tail.Prelude.tail) f)).Data.Text.lines<$>T.readFile (Prelude.head file)
      if dayToWeek td /= ((toEnum :: Int -> Week) getWeek) then loop pd td ptfunc (Prelude.tail file)
      else ( do
       (ft,lt) <- getNum ':' time (25,61)
       case makeTimeOfDayValid ft lt 0 of
        Nothing  -> loop pd td ptfunc (Prelude.tail file)
        Just tod -> ( do
         postid_str <- ptfunc text pd []
         ctz <- getCurrentTimeZone
         if Data.Text.null postid_str then loop pd td ptfunc (Prelude.tail file)
         else loop pd{schedule = (postid_str, ZonedTime{zonedTimeZone = ctz, zonedTimeToLocalTime = td{localTimeOfDay = tod}}):(schedule pd)} 
                   td ptfunc (Prelude.tail file)))))

dayToWeek :: LocalTime -> Week
dayToWeek day = do
 let [y,m,d] = ((\(a,b,c) -> [a+(div(toInteger b+12)15)-1,(toInteger b+12-12*div(12+toInteger b)15),toInteger c]).toGregorian.localDay) day
 ((toEnum :: Int -> Week).fromIntegral) (mod(d+div(26*(m+1))10+mod y 100+div(mod y 100)4+5*div y 100+div(div y 100)4+5)7)
