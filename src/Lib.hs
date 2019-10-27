module Lib where

import SlackAPI
import TwitterAPI
import Control.Exception
import Control.Concurrent
import Control.Directory
import System.IO
import Control.Exception
import Data.Time
import qualified Data.List as L
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Vector as V

data PostData = PostData { calcweb :: [String] -- calc's post before
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

--permissionIndexes :: [T.Text] -> [User] -> Int -> [Int]
--permissionIndexes dm puser index 
-- | null dm                                    = []  
-- | head dm `elem` (map gid_str puser) = index:permissionIndexes (tail dm) puser (index + 1)
-- | otherwise                                          = permissionIndexes (tail dm) puser (index + 1)
--
--getPermitFromIndex :: [Int] -> [GetMessageCreate] -> [GetMessageCreate]
--getPermitFromIndex ind mcs = if null ind then [] else (mcs!!head ind):getPermitFromIndex (tail ind) mcs
--
--dmTotext = gettext.getmessage_data.getmessage_create
--textTolistlisttext = map T.words.T.lines
--listlisttextTotext = T.init.T.unlines.map T.unwords
--parampart = head.head.textTolistlisttext.dmTotext
--texttwopart = listlisttextTotext.(\n->(tail.head)n:(tail n)).textTolistlisttext.dmTotext
--textothpart = listlisttextTotext.(\n->(tail.tail.head)n:(tail n)).textTolistlisttext.dmTotext
--numpart = read.T.unpack.T.tail.head.tail.T.words.dmTotext

monitoring :: MVar V.Vector GetMention -> PostData -> [String] -> IO PostData
monitoring msgq pd botconf = do
 threadDelay(mentiont)
 postdata <- (rtCheck pd >>= remindCheck typeTL)-- monitoring retweeting
 pusr <- getPermitUser 1
 tlmention <- (\t -> case t of Left  e -> error e
                               Right l -> (V.reverce.V.fromList) l <$> getMention (pack "") botconf
 if (gm_str.V.head) tlmention == befts pd then monitoring msgq pd
 else do
  nowq <- readMVar msgq 
  if V.null nowq then do
   putMVar msgq tlmention
   forkIO $ cmdCheck msgq botconf pd
  else do
   befq <- takeMVar msgq 
   putMVar (befq V.++ (V.filter (\x -> filterUserElem x puser && filterCmdCalcTweet x) tlmention)) -- user and command checking
  monitoring msgq postdata
 where
  filterUserElem x   = V.or.V.map (V.elem ((gid_str.gmuser) x))
  filterCmdCalcTweet = (=="calc-tweet").Prelude.head.Prelude.head.Prelude.map T.words.T.lines.gmid_str

cmdCheck :: MVar V.Vector GetMention -> [String] -> PostData -> IO PostData
cmdCheck msgq botconf postdata = 
 nowq <- readMVar msgq  
 if V.null nowq then return postdata
 else do
  pd <- (case filterCmd nowq 1 of
              "tweet" -> tweetCmd 
              "user"  -> userCmd
              "web"   -> webCmd
              _       -> errorCmd) V.head nowq botconf postdata 
  takeMVar msgq >>= \x -> (putMVar msgq.V.tail) x
  cmdCheck msgq botconf pd

tweetCmd :: GetMention -> [String] -> PostData -> IO PostData
tweetCmd msg botconf postdata = (case filterCmd msg 2 of
 "post"      -> twpsCmd 
 "broadcast" -> twbdCmd 
 "rm"        -> twrmCmd 
 "set"       -> twstCmd 
 _           -> errorCmd) msg botconf postdata

userCmd ::GetMention -> [String] -> PostData -> IO PostData
userCmd msg botconf postdata = (case filterCmd msg 2 of
 "add"       -> uaddCmd
 "rm"        -> urmCmd 
 "broadcast" -> ubrCmd
 _           -> errorCmd) msg botconf postdata

webCmd :: GetMention -> [String] -> PostData -> IO PostData
webCmd msg botconf postdata = 

errorCmd :: GetMention -> [String] -> PostData -> IO PostData
errorCmd msg botconf postdata = 

filterCmd vmsgq n = (Prelude.!! n.Prelude.head.Prelude.map T.words.T.lines.gmid_str.V.head) vmsgq 

getPermitUser :: Int -> IO [T.Text]
getPermitUser 1 = (\p -> case p of Left  e -> error e -- pusermittions
                                   Right l ->(Prelude.map ((\(a,b,c,d) ->a).L.splitOn ",")T.lines) l <$> TIO.readFile permitconf)
getPermitUser 2 = (\p -> case p of Left  e -> error e -- pusermittions
                                   Right l ->(Prelude.map ((\(a,b,c,d) ->b).L.splitOn ",")T.lines) l <$> TIO.readFile permitconf)
getPermitUser 3 = (\p -> case p of Left  e -> error e -- pusermittions
                                   Right l ->(Prelude.map ((\(a,b,c,d) ->c).L.splitOn ",")T.lines) l <$> TIO.readFile permitconf)
getPermitUser 4 = (\p -> case p of Left  e -> error e -- pusermittions
                                   Right l ->(Prelude.map ((\(a,b,c,d) ->d).L.splitOn ",")T.lines) l <$> TIO.readFile permitconf)
getPermitUser _ = error "getPermitUser :: Int -> IO [T.Text]"

setPostData :: (T.Text, [String],[(T.Text,ZonedTime)], Bool) -> PostData
setPostData (beforets, web, sche, non) = PostData { befts = beforets, calcweb = web , schedule = sche, noon = non }

--postTweet :: PostData -> [GetMessageCreate] -> (T.Text -> PostData -> [GetMessageCreate] -> IO T.Text) -> IO PostData
--postTweet postdata tw ptfunc= do 
-- let ntdata = createNoticeData (takeWhile (((\x -> (x/=(T.pack "$clear")) && (x/=(T.pack "$post"))).head.head.textTolistlisttext.dmTotext)) tw)
--                               NoticeData{notice = T.pack "", date = [], time = [], locale =[]}
-- if (T.null.notice) ntdata then return postdata
-- else (do
--  posttw <- TIO.readFile noticetempconf
--  let posttx = makeTweet ntdata 1 ((maximum.map (maximum.map snd.((T.pack "null",0):)))[date ntdata, time ntdata, locale ntdata]) 
--                                                                (T.append posttw (T.append (notice ntdata) (T.pack "\n")))
--  postid_str <- ptfunc posttx postdata tw
--  if T.null postid_str then return postdata
--  else setNoticeTime  postdata ntdata postid_str >>= (\r->return postdata {schedule = r}) )

calcWebPost :: PostData -> [GetMessageCreate] -> (T.Text -> PostData -> [GetMessageCreate] -> IO T.Text) -> IO PostData
calcWebPost postdata tw ptfunc= do
 nowpost <- getDirectoryContents srvcalcdir
 let newarticle = filter (\x->x `notElem` (calcweb postdata)) nowpost
 if null newarticle then return postdata
 else loop newarticle nowpost
  where
   loop :: [String] -> [String] -> IO PostData
   loop na np = if null na then return postdata { calcweb = np }
                else ( do
                 article <- T.lines<$>TIO.readFile (calcwebdir ++ ((takeWhile (/= '.')).head) na ++ ".md")
                 let title  = T.drop 7 (article!!1)
                     author = (article!!2)
                     webtx  = T.pack $ T.unpack author ++ "\n" 
                                     ++ T.unpack title ++ "について書きました。\n url: https://calc.mie.jp/posts/" 
                                     ++ head na
                 ptfunc webtx postdata tw
                 loop (tail na) np )
  
createNoticeData :: [GetMessageCreate] -> NoticeData -> NoticeData
createNoticeData messages ntdata = if null messages || noticeAll ntdata then ntdata else createNoticeData (tail messages) (
  case (T.unpack.parampart.head) messages of
   "$notice" -> if (T.null.notice) ntdata then ntdata{notice = (texttwopart.head) messages} else ntdata
   "$date"   -> if ((snd.gmcToNd.head) messages) `elem` (map snd (date ntdata)) then ntdata else ntdata { date = ((gmcToNd.head) messages):date ntdata }
   "$time"   -> if ((snd.gmcToNd.head) messages) `elem` (map snd (time ntdata)) then ntdata else ntdata { time = ((gmcToNd.head) messages):time ntdata }
   "$locale" -> if ((snd.gmcToNd.head) messages) `elem` (map snd (locale ntdata)) then ntdata else ntdata { locale = ((gmcToNd.head) messages):locale ntdata }
   _         -> ntdata
  )


gmcToNd :: GetMessageCreate -> (T.Text,Int)
gmcToNd message = if (T.head.(!!1).head.textTolistlisttext.dmTotext) message == '-' then  (textothpart message, numpart message) else (texttwopart message, 1)

noticeAll :: NoticeData -> Bool
noticeAll ntdata = if (T.null.notice) ntdata || (null.date) ntdata || (null.time) ntdata || (null.locale) ntdata then False else True
 
--makeTweet :: NoticeData -> Int -> Int -> T.Text -> T.Text
--makeTweet ntdata n mx tw = if n>mx then tw else 
-- makeTweet ntdata (n+1) mx (if n `elem` concatMap (map snd) [date ntdata, time ntdata, locale ntdata] then
--                             T.append tw (
--                             T.append (elemText n (date ntdata)) (
--                             T.append (elemText n (time ntdata)) (
--                             T.append (if (T.null.elemText n) (locale ntdata) then T.pack "" 
--                                                else T.append (T.pack "＠.") (elemText n (locale ntdata))) (T.pack "\n"))))  -- createSchedule
--                             else tw)

elemText :: Int -> [(T.Text, Int)] -> T.Text
elemText n text = if (n `notElem` map snd text) then T.pack "" 
                        else T.append ((fst.head.filter ((==n).snd)) text) (T.pack " ")

--userAdd :: PostData -> [GetMessageCreate] -> IO PostData
--userAdd postdata tw = do
-- let user = ((T.drop 9.gettext.getmessage_data.getmessage_create.head) tw)
-- permituser <- T.lines<$>TIO.readFile permitconf
-- if user `notElem` permituser then (TIO.appendFile permitconf user >> return postdata)
-- else return postdata 

setNoticeTime :: PostData -> NoticeData -> T.Text -> IO [(T.Text,ZonedTime)]
setNoticeTime pdt ndt res = loop 1 ((maximum.map (maximum.map snd.((T.pack "null",0):)))[date ndt, time ndt]) [date ndt, time ndt] pdt
 where 
   loop :: Int -> Int -> [[(T.Text,Int)]] -> PostData -> IO[(T.Text,ZonedTime)]
   loop n mx [dat, tim] pdt
    | n>mx = (return.schedule) pdt 
    | all (elem n.map snd) [dat, tim] = do
       ctz     <- getCurrentTimeZone
       (ft,lt) <- getNum ':' ((fst.head.filter ((==n).snd)) tim) (25,61)
       (fd,ld) <- getNum '/' ((fst.head.filter ((==n).snd)) dat) (13,32)
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
 let rtlist = filter ((<=15*60).((`diffUTCTime` now).zonedTimeToUTC.snd)) (schedule postdata)
 case null rtlist of
  True  -> return postdata 
  False -> loop rtlist postdata
   where 
    loop rtl pdt = if null rtl then
                    return (setPostData ( befts pdt
                                        , calcweb pdt 
                                        , filter ((>15*60).((`diffUTCTime` now).zonedTimeToUTC.snd)) (schedule pdt)
                                        , noon pdt))
                   else  
                    (do
                      postRT ((fst.head) rtl) 
                      loop (tail rtl) pdt )

remindCheck :: (T.Text -> PostData -> [GetMessageCreate] -> IO T.Text) ->  PostData -> IO PostData
remindCheck ptfunc postdata = do
 today <- zonedTimeToLocalTime<$>getZonedTime
 case divMod ((todHour.localTimeOfDay) today) 12 of
  (0,_)  -> return postdata { noon = False }
  (1,0)  -> getDirectoryContents reminddir >>= (\fs -> loop postdata today ptfunc (map (reminddir++) fs))
  (1,_)  -> return postdata { noon = True }
  where
   loop :: PostData -> LocalTime -> (T.Text -> PostData -> [GetMessageCreate] -> IO T.Text) -> [FilePath] -> IO PostData
   loop pd td ptfunc file = if null file || noon pd then return pd { noon = True }
    else (doesFileExist.head) file >>= 
     (\check -> if not check then loop pd td ptfunc (tail file)
     else ( do
      (getWeek, time, text)<-(\f->((read.T.unpack.head) f
                                  ,(head.tail) f
                                  ,(T.unlines.tail.tail) f)).T.lines<$>TIO.readFile (head file)
      if dayToWeek td /= ((toEnum :: Int -> Week) getWeek) then loop pd td ptfunc (tail file)
      else ( do
       (ft,lt) <- getNum ':' time (25,61)
       case makeTimeOfDayValid ft lt 0 of
        Nothing  -> loop pd td ptfunc (tail file)
        Just tod -> ( do
         postid_str <- ptfunc text pd []
         ctz <- getCurrentTimeZone
         if T.null postid_str then loop pd td ptfunc (tail file)
         else loop pd{schedule = (postid_str, ZonedTime{zonedTimeZone = ctz, zonedTimeToLocalTime = td{localTimeOfDay = tod}}):(schedule pd)} 
                   td ptfunc (tail file)))))

dayToWeek :: LocalTime -> Week
dayToWeek day = do
 let [y,m,d] = ((\(a,b,c) -> [a+(div(toInteger b+12)15)-1,(toInteger b+12-12*div(12+toInteger b)15),toInteger c]).toGregorian.localDay) day
 ((toEnum :: Int -> Week).fromIntegral) (mod(d+div(26*(m+1))10+mod y 100+div(mod y 100)4+5*div y 100+div(div y 100)4+5)7)
