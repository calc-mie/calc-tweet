module Lib where

import SlackAPI
import TwitterAPI
import Control.Exception
import Control.Concurrent
import Data.List
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Time
import System.Directory

data PostData = PostData { befts :: T.Text
                         , calcweb :: [String] -- calc's post before
                         , schedule :: [(T.Text, ZonedTime)] -- post text (twid, time)
                         , noon :: Bool
                         } deriving (Show)

data NoticeData = NoticeData { notice :: T.Text
                             , date :: [(T.Text,Int)]
                             , time :: [(T.Text,Int)]
                             , locale :: [(T.Text,Int)]
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


permissionIndexes :: [T.Text] -> [User] -> Int -> [Int]
permissionIndexes dm puser index 
 | Prelude.null dm                                    = []  
 | Prelude.head dm `elem` (Prelude.map gid_str puser) = index:permissionIndexes (Prelude.tail dm) puser (index + 1)
 | otherwise                                          = permissionIndexes (Prelude.tail dm) puser (index + 1)

getPermitFromIndex :: [Int] -> [GetMessageCreate] -> [GetMessageCreate]
getPermitFromIndex ind mcs = if Prelude.null ind then [] else (mcs!!Prelude.head ind):getPermitFromIndex (Prelude.tail ind) mcs

dmTotext = gettext.getmessage_data.getmessage_create
textTolistlisttext = Prelude.map T.words.T.lines
listlisttextTotext = T.init.T.unlines.Prelude.map T.unwords
parampart = head.head.textTolistlisttext.dmTotext
texttwopart = listlisttextTotext.(\n->(Prelude.tail.Prelude.head)n:(Prelude.tail n)).textTolistlisttext.dmTotext
textothpart = listlisttextTotext.(\n->(Prelude.tail.Prelude.tail.Prelude.head)n:(Prelude.tail n)).textTolistlisttext.dmTotext
numpart = read.T.unpack.T.tail.Prelude.head.Prelude.tail.T.words.dmTotext

setPostData :: (T.Text, [String],[(T.Text,ZonedTime)], Bool) -> PostData
setPostData (beforets, web, sche, non) = PostData { befts = beforets, calcweb = web , schedule = sche, noon = non }

postTweet :: PostData -> [GetMessageCreate] -> (T.Text -> PostData -> [GetMessageCreate] -> IO T.Text) -> IO PostData
postTweet postdata tw ptfunc= do 
 let ntdata = createNoticeData (dropWhile ((/=(T.pack "$post")).head.head.textTolistlisttext.dmTotext) tw)
                               NoticeData{notice = T.pack "", date = [], time = [], locale =[]}
 if (T.null.notice) ntdata then return postdata
 else (do
  posttw <- TIO.readFile noticetempconf
  let posttx = makeTweet ntdata 1 ((Prelude.maximum.Prelude.map (Prelude.maximum.Prelude.map snd.((T.pack "null",0):)))[date ntdata, time ntdata, locale ntdata]) 
                                                                (T.append posttw (T.append (notice ntdata) (T.pack "\n")))
  postid_str <- ptfunc posttx postdata tw
  if T.null postid_str then return postdata
  else setNoticeTime  postdata ntdata postid_str >>= (\r->return postdata {schedule = r}) )

calcWebPost :: PostData -> [GetMessageCreate] -> (T.Text -> PostData -> [GetMessageCreate] -> IO T.Text) -> IO PostData
calcWebPost postdata tw ptfunc= do
 nowpost <- getDirectoryContents srvcalcdir
 let newarticle = Prelude.filter (\x->x `notElem` (calcweb postdata)) nowpost
 if Prelude.null newarticle then return postdata
 else loop newarticle nowpost
  where
   loop :: [String] -> [String] -> IO PostData
   loop na np = if Prelude.null na then return postdata { calcweb = np }
                else ( do
                 article <- T.lines<$>TIO.readFile (calcwebdir ++ ((Prelude.takeWhile (/= '.')).Prelude.head) na ++ ".md")
                 let title  = T.drop 7 (article!!1)
                     author = (article!!2)
                     webtx  = T.pack $ T.unpack author ++ "\n" 
                                     ++ T.unpack title ++ "について書きました。\n url: https://calc.mie.jp/posts/" 
                                     ++ Prelude.head na
                 ptfunc webtx postdata tw
                 loop (Prelude.tail na) np )
  
createNoticeData :: [GetMessageCreate] -> NoticeData -> NoticeData
createNoticeData messages ntdata = if Prelude.null messages || noticeAll ntdata then ntdata else createNoticeData (Prelude.tail messages) (
 case (T.unpack.parampart.head) messages of
  "$notice" -> ntdata{notice = (texttwopart.head) messages}
  "$date"   -> ntdata{date = (gmcToNd.head) messages:date ntdata}
  "$time"   -> ntdata{time = (gmcToNd.head) messages:time ntdata}
  "$locale" -> ntdata{locale = (gmcToNd.head) messages:locale ntdata}
 )

gmcToNd :: GetMessageCreate -> (T.Text,Int)
gmcToNd message = if (T.head.(!!1).head.textTolistlisttext.dmTotext) message == '-' then (textothpart message, numpart message) else (texttwopart message, 1)



noticeAll :: NoticeData -> Bool
noticeAll ntdata = if (T.null.notice) ntdata && (null.date) ntdata && (null.time) ntdata && (null.locale) ntdata then False else True
 
makeTweet :: NoticeData -> Int -> Int -> T.Text -> T.Text
makeTweet ntdata n mx tw = if n>mx then tw else 
 makeTweet ntdata (n+1) mx (if n `elem` Prelude.concatMap (Prelude.map snd) [date ntdata, time ntdata, locale ntdata] then
                             T.append tw (
                             T.append (elemText n (date ntdata)) (
                             T.append (elemText n (time ntdata)) (
                             T.append (if (T.null.elemText n) (locale ntdata) then T.pack "" 
                                                else T.append (T.pack "＠.") (elemText n (locale ntdata))) (T.pack "\n"))))  -- createSchedule
                             else tw)

elemText :: Int -> [(T.Text, Int)] -> T.Text
elemText n text = if (n `notElem` Prelude.map snd text) then T.pack "" 
                        else T.append ((fst.Prelude.head.Prelude.filter ((==n).snd)) text) (T.pack " ")

userAdd :: PostData -> [GetMessageCreate] -> IO PostData
userAdd postdata tw = do
 let user = ((T.drop 9.gettext.getmessage_data.getmessage_create.Prelude.head) tw)
 permituser <- T.lines<$>TIO.readFile permitconf
 if user `notElem` permituser then (TIO.appendFile permitconf user >> return postdata)
 else return postdata 

setNoticeTime :: PostData -> NoticeData -> T.Text -> IO [(T.Text,ZonedTime)]
setNoticeTime pdt ndt res = loop 1 ((Prelude.maximum.Prelude.map (Prelude.maximum.Prelude.map snd.((T.pack "null",0):)))[date ndt, time ndt]) [date ndt, time ndt] pdt
 where 
   loop :: Int -> Int -> [[(T.Text,Int)]] -> PostData -> IO[(T.Text,ZonedTime)]
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

getNum :: Char -> T.Text -> (Int,Int) -> IO(Int,Int)
getNum c text (f,l) = if T.null text then return (f,l) else case (elemIndex c.T.unpack.T.filter (\x->x `elem` (c:['0'..'9']))) text of
 Nothing -> return (f,l)
 Just n  -> catch(
             (return.(\(a,b)->((read.T.unpack) a,(read.T.unpack.T.take 2.T.tail) b)).T.splitAt n.T.filter (\x->x `elem` (c:['0'..'9']))) text)
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
                    return (setPostData ( befts pdt
                                        , calcweb pdt 
                                        , Prelude.filter ((>15*60).((`diffUTCTime` now).zonedTimeToUTC.snd)) (schedule pdt)
                                        , noon pdt))
                   else  
                    (do
                      postRT ((fst.Prelude.head) rtl) 
                      loop (Prelude.tail rtl) pdt )

remindCheck :: (T.Text -> PostData -> [GetMessageCreate] -> IO T.Text) ->  PostData -> IO PostData
remindCheck ptfunc postdata = do
 today <- zonedTimeToLocalTime<$>getZonedTime
 case divMod ((todHour.localTimeOfDay) today) 12 of
  (0,_)  -> return postdata { noon = False }
  (1,0)  -> getDirectoryContents reminddir >>= (\fs -> loop postdata today ptfunc (Prelude.map (reminddir++) fs))
  (1,_)  -> return postdata { noon = True }
  where
   loop :: PostData -> LocalTime -> (T.Text -> PostData -> [GetMessageCreate] -> IO T.Text) -> [FilePath] -> IO PostData
   loop pd td ptfunc file = if Prelude.null file || noon pd then return pd { noon = True }
    else (doesFileExist.Prelude.head) file >>= 
     (\check -> if not check then loop pd td ptfunc (Prelude.tail file)
     else ( do
      (getWeek, time, text)<-(\f->((read.T.unpack.Prelude.head) f
                                  ,(Prelude.head.Prelude.tail) f
                                  ,(T.unlines.Prelude.tail.Prelude.tail) f)).T.lines<$>TIO.readFile (Prelude.head file)
      if dayToWeek td /= ((toEnum :: Int -> Week) getWeek) then loop pd td ptfunc (Prelude.tail file)
      else ( do
       (ft,lt) <- getNum ':' time (25,61)
       case makeTimeOfDayValid ft lt 0 of
        Nothing  -> loop pd td ptfunc (Prelude.tail file)
        Just tod -> ( do
         postid_str <- ptfunc text pd []
         ctz <- getCurrentTimeZone
         if T.null postid_str then loop pd td ptfunc (Prelude.tail file)
         else loop pd{schedule = (postid_str, ZonedTime{zonedTimeZone = ctz, zonedTimeToLocalTime = td{localTimeOfDay = tod}}):(schedule pd)} 
                   td ptfunc (Prelude.tail file)))))

dayToWeek :: LocalTime -> Week
dayToWeek day = do
 let [y,m,d] = ((\(a,b,c) -> [a+(div(toInteger b+12)15)-1,(toInteger b+12-12*div(12+toInteger b)15),toInteger c]).toGregorian.localDay) day
 ((toEnum :: Int -> Week).fromIntegral) (mod(d+div(26*(m+1))10+mod y 100+div(mod y 100)4+5*div y 100+div(div y 100)4+5)7)
