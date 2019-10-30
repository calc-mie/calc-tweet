module Lib where

import SlackAPI
import TwitterAPI

import Control.Exception
import Control.Concurrent
import System.Directory
import System.IO
import Control.Exception
import Data.Time
import qualified Data.List as L
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Vector as V

-- have to MVector
data PostQueue = PostQueue { mentions :: V.Vector GetMention
                           , schedule :: V.Vector (T.Text, ZonedTime) -- twid, retweet time
                           } deriving (Show)

--data PostData = PostData { calcweb :: [String] -- calc's post before
--                         , schedule :: [(T.Text, ZonedTime)] -- post text (twid, time)
--                         , noon :: Bool
--                         } deriving (Show)

--data NoticeData = NoticeData { notice :: T.Text
--                             , date :: [(T.Text,Int)]
--                             , time :: [(T.Text,Int)]
--                             , locale :: [(T.Text,Int)]
--                             } deriving (Show)

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

emptyint = 1*1000*1000 {- 1 second -}
mentiont = 12*1000*1000 {-12 second -}

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
 
-- case directmessage of 
--  Left err -> monitoring postdata
--  Right dm -> if befts pd == (getcreated_timestamp . head . getevents) dm 
--               then monitoring postdata
--              else do
--               pusr <- (TIO.readFile permitconf >>= getUser.T.intercalate (T.pack ",").T.lines)
--               case pusr of
--                Left err             -> monitoring postdata
--                Right permissionuser -> ( do
--                 let puser = permissionIndexes ((map sender_idpart) ((getevents) dm)) permissionuser 0
--                 cmdCheck (postdata{befts = (getcreated_timestamp . head . getevents) dm }) ((V.fromList.getevents) dm) (
--                  case elemIndex (befts pd) (map getcreated_timestamp (getevents dm)) of 
--                   Nothing -> (length.map getcreated_timestamp) (getevents dm)
--                   Just n  -> (n-1) ) >>= monitoring )

   
--cmdCheck :: PostData -> V.Vector GetMessageCreate -> Int -> IO PostData 
--cmdCheck postdata tw n
-- | n < 0                 = return postdata
-- | otherwise             = 
--  case (T.unpack.head.head.map T.words.T.lines.gettext.getmessage_data.getmessage_create) (tw V.! n) of
--   "$post"          -> postTweet postdata ((filter ((==sender_idpart (tw V.! n)).sender_idpart).V.toList.V.drop (n+1)) tw) typeTL >>= (\ret -> cmdCheck ret tw (n-1))
--   "$print"         -> postTweet postdata ((filter ((==sender_idpart (tw V.! n)).sender_idpart).V.toList.V.drop (n+1)) tw) typeDM >>= (\ret -> cmdCheck ret tw (n-1))
--   "$post-calc-web" -> calcWebPost postdata ((V.toList.V.drop (n+1)) tw) typeTL >>= (\ret -> cmdCheck ret tw (n-1))
--   "$useradd"       -> userAdd postdata ((V.toList.V.drop (n+1)) tw) >>= (\ret -> cmdCheck ret tw (n-1))
--   _                -> cmdCheck postdata tw (n-1)
--
--sender_idpart = getsender_id.getmessage_create


monitoring :: MVar PostQueue -> [String] -> IO ()
monitoring msgq botconf = do
 threadDelay(mentiont)
 (rtCheck pd >>= remindCheck typeTL)-- monitoring retweeting
 tlmention <- (\t -> case t of Left  e -> error e
                               Right l -> (V.reverse.V.fromList) l) <$> getMention (pack "") botconf
 if (gm_str.V.head) tlmention then monitoring msgq botconf
 else do
  nowq <- readMVar msgq 
  if (V.null.mentions) nowq then do
   putMVar msgq nowq {mentions = (V.filter (\x -> filterUserElem x puser && filterCmdCalcTweet x)) tlmention }
   forkIO $ cmdCheck msgq botconf 
  else do
   befq <- (mentions.takeMVar) msgq 
   putMVar befq {mentions = (befq V.++ (V.filter (\x -> filterUserElem x puser && filterCmdCalcTweet x) tlmention))} -- user and command checking
  monitoring msgq botconf
 where
  filterUserElem x   = (V.or.V.map (V.elem (gid_str.gmuser)) x)
  filterCmdCalcTweet = (=="calc-tweet").Prelude.head.Prelude.head.Prelude.map T.words.T.lines.gmid_str

cmdCheck :: MVar PostQueue -> [String] -> IO ()
cmdCheck msgq botconf = readMVar msgq >>= \nowq -> if (V.null.mentions) nowq then return () else 
 do
  sc <- (case filterCmd nowq 1 of
              "tweet" -> tweetCmd 
              "user"  -> userCmd
--            "web"   -> webCmd
              _       -> errorCmd) nowq botconf
  addDeleteSchedule msgq sc  -- add or deleteschedule 
  cmdCheck msgq botconf
   where
    addDeleteSchedule q d = takeMVar msgq >>= \x -> putMVar msgq x{mentions = (V.tail.menitons)x
                                                                   ,schedule =  if V.null d then schedule x else schedule x V.++ d} 

-- tweet command 
tweetCmd :: PostQueue -> [String] -> IO V.Vector (T.Text, ZonedTime)
tweetCmd msg botconf = (case filterCmd msg 2 of
 "post"      -> twpostCmd
 "broadcast" -> twbroadCmd 
 "rm"        -> twrmCmd 
 "help"      -> twHelpCmd
--  "set"       -> twsetCmd 
 _           -> errorCmd) msg botconf postdata

twpostCmd :: PostQueue -> [String] -> IO V.Vector (T.Text, ZonedTime)
twpostCmd msg botconf = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 1 >>= \x -> if not x then return 0 else do
 let user_id  = (gid_str.gmuser.V.head.mentions) msg    -- twitter api name
     since_id = (T.pack.show) $ (read.!! 3.Prelude.head.Prelude.map T.words.T.lines.gmid_str.V.head) msg - 1 -- twitter api name 
 userTL <- (\t -> case t of  Left e  -> error e
                             Right l -> V.fromList l) <$> getUserTL user_id since_id botconf
 let postmsg = searchReplyTree userTL -- search and sed
 postTweet postmsg [] botconf 
 return ()

twbroadcastCmd :: PostQueue -> [String] -> IO V.Vector (T.Text, ZonedTime)
twbroadcastCmd msg botconf = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 1 >>= \x -> if not x then return 0 else do
 let user_id  = (gid_str.gmuser.V.head.mentions) msg    -- twitter api name
     since_id = (T.pack.show) $ (read.!! 3.Prelude.head.Prelude.map T.words.T.lines.gmid_str.V.head) msg - 1 -- twitter api name
 userTL <- (\t -> case t of  Left e  -> error e
                             Right l -> V.fromList l) <$> getUserTL user_id since_id botconf
 let postmsg = searchReplyTree userTL
 postTarget <- broadUser -- search and sed
 postTweet postmsg postTarget botconf 
 return ()

twrmCmd :: PostQueue -> [String] -> IO V.Vector (T.Text, ZonedTime)
twrmCmd msg botconf = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 1 >>= \x -> if not x then return () else do
 let user_id  = T.pack "calc-mie"    -- twitter api name
     since_id = (T.pack.show) $ (read.!! 3.Prelude.head.Prelude.map T.words.T.lines.gmid_str.V.head) msg - 1 -- twitter api name
 userTL <- (\t -> case t of  Left e  -> error e
                             Right l -> V.fromList l) <$> getUserTL user_id since_id botconf
-- searchRmTweet 
 rmTweet postTarget [] botconf
 return ()
  where
--   searchRmTweet

twHelpCmd :: PostQueue -> [String] -> IO V.Vector (T.Text, ZonedTime)
twHelpCmd msg botconf = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 1 >>= \x -> if not x then return () else do
 let user_id  = (gid_str.gmuser.V.head.mentions) msg
 let postTarget = twHelpComment
 postTweet postTarget user_id botconf
 return ()

-- comming soon?
--twsetCmd :: PostQueue -> [String] -> IO V.Vector (T.Text, ZonedTime)
--twsetCmd msg botconf = do

-- user command
userCmd :: PostQueue -> [String] -> IO V.Vector (T.Text, ZonedTime)
userCmd msg botconf = (case filterCmd msg 2 of
 "add"       -> uaddCmd
 "rm"        -> urmCmd 
 "set"       -> usetCmd
 "help"      -> uhelpCmd
 _           -> errorCmd) msg botconf

uaddCmd :: PostQueue -> [String] -> IO V.Vector (T.Text, ZonedTime)
uaddCmd msg botconf = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 2 >>= \x -> if not x then return () else do
 let users  = (reDup.Prelude.drop 3.Prelude.head.Prelude.map T.words.T.lines.gmid_str.V.head) msg
 pusers <- (\(a,b,c,d) -> a).Prelude.map T.words.T.lines <$> TIO.readFile premitconf
 return ()
-- if Prelude.map (`notElem`permituser) users then (TIO.appendFile permitconf users)
--  where
--   rmDup = foldl (\seen x -> if x `elem` seen then seen else x:seen) []  

urmCmd :: PostQueue -> [String] -> IO V.Vector (T.Text, ZonedTime)
urmCmd msg botconf = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 2 >>= \x -> if not x then return () else do
 let user_id  = (gid_str.gmuser.V.head.mentions) msg
 let postTarget = twHelpComment
 postTweet postTarget user_id botconf
 return ()
 
usetCmd :: PostQueue -> [String] -> IO V.Vector (T.Text, ZonedTime)
usetCmd msg botconf = (case filterCmd msg 3 of 
 "post"      -> usetPostCmd
 "sudo"      -> usetSudoCmd
 "broadcast" -> usetBroadcastCmd
 _           -> errorCmd) msg botconf

usetPostCmd :: PostQueue -> [String] -> IO V.Vector (T.Text, ZonedTime)
usetPostCmd = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 2 >>= \x -> if not x then return () else do
 let user   = (!! 4.Prelude.head.Prelude.map T.words.T.lines.gmid_str.V.head) msg
     permit = (!! 5.Prelude.head.Prelude.map T.words.T.lines.gmid_str.V.head) msg
 return()

usetSudoCmd :: PostQueue -> [String] -> IO V.Vector (T.Text, ZonedTime)
usetSudoCmd = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 2 >>= \x -> if not x then return () else do
 let user   = (!! 4.Prelude.head.Prelude.map T.words.T.lines.gmid_str.V.head) msg
     permit = (!! 5.Prelude.head.Prelude.map T.words.T.lines.gmid_str.V.head) msg
 return()

usetBroadcastCmd :: PostQueue -> [String] -> IO V.Vector (T.Text, ZonedTime)
usetBroadcastCmd = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 2 >>= \x -> if not x then return () else do
 let user   = (!! 4.Prelude.head.Prelude.map T.words.T.lines.gmid_str.V.head) msg
     permit = (!! 5.Prelude.head.Prelude.map T.words.T.lines.gmid_str.V.head) msg
 return()

uhelpCmd :: PostQueue -> [String] -> IO V.Vector (T.Text, ZonedTime)
uhelpCmd msg botconf = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 1 >>= \x -> if not x then return () else do
 let user_id  = (gid_str.gmuser.V.head.mentions) msg
 let postTarget = uHelpComment
 postTweet postTarget user_id botconf
 return ()

-- web command comming soon?
--webCmd :: MVar PostQueue -> [String] -> IO V.Vector (T.Text, ZonedTime) -- post web
--webCmd msg botconf postdata = calcWebPost postdata msg botconf typeTL

-- command error post
--errorCmd :: PostQueue -> [String] -> IO V.Vector (T.Text, ZonedTime) -- post error
--errorCmd msg botconf =

filterCmd vmsgq n = (!! n.Prelude.head.Prelude.map T.words.T.lines.gmid_str.V.head) vmsgq 

getPermitUser :: T.Text -> Int -> IO Bool
getPermitUser uid n = if n <= 0 || 4 < n then error "getPermitUser :: error" else do
 users <- (\p -> case p of Left  e -> error e -- permition to use calc-tweet 
                           Right l ->(Prelude.map (T.splitOn ",").T.lines) l <$> TIO.readFile permitconf)
 ((\p -> case p of Nothing -> False
                   Just a  -> if n == 1 then strToBool b else -- tweet
                              if n == 2 then strToBool c else -- sudo
                              if n == 3 then strToBool d else -- broadcast
                                 False).find (\(a,b,c,d) -> uid == a)) users
 where
  strToBool str = if str == T.pack "True" then True else False

broadUsers :: IO [T.Text]
broadUsers = TIO.readFile permitconf >>= (\(a,b,c,d) -> a).Prelude.filter (\(a,b,c,d) -> d) 

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

calcWebPost :: PostData -> GetMention -> [String] -> (T.Text -> PostData -> [GetMessageCreate] -> IO T.Text) -> IO PostData
calcWebPost postdata tw botconf ptfunc= do
 nowpost <- getDirectoryContents srvcalcdir
 let newarticle = filter (\x->x `notElem` (calcweb postdata)) nowpost
 if null newarticle then return postdata
 else loop newarticle nowpost botconf
  where
   loop :: [String] -> [String] -> [String] -> IO PostData
   loop na np conf =
    if null na then return postdata { calcweb = np }
    else ( do
      article <- T.lines<$>TIO.readFile (calcwebdir ++ ((takeWhile (/= '.')).head) na ++ ".md")
      let title  = T.drop 7 (article!!1)
          author = (article!!2)
          webtx  = T.pack $ T.unpack author ++ "\n" 
                          ++ T.unpack title ++ "について書きました。\n url: https://calc.mie.jp/posts/" 
                          ++ head na
      ptfunc webtx postdata tw conf
      loop (tail na) np )
  
--createNoticeData :: [GetMessageCreate] -> NoticeData -> NoticeData
--createNoticeData messages ntdata = if null messages || noticeAll ntdata then ntdata else createNoticeData (tail messages) (
--  case (T.unpack.parampart.head) messages of
--   "$notice" -> if (T.null.notice) ntdata then ntdata{notice = (texttwopart.head) messages} else ntdata
--   "$date"   -> if ((snd.gmcToNd.head) messages) `elem` (map snd (date ntdata)) then ntdata else ntdata { date = ((gmcToNd.head) messages):date ntdata }
--   "$time"   -> if ((snd.gmcToNd.head) messages) `elem` (map snd (time ntdata)) then ntdata else ntdata { time = ((gmcToNd.head) messages):time ntdata }
--   "$locale" -> if ((snd.gmcToNd.head) messages) `elem` (map snd (locale ntdata)) then ntdata else ntdata { locale = ((gmcToNd.head) messages):locale ntdata }
--   _         -> ntdata
--  )


--gmcToNd :: GetMessageCreate -> (T.Text,Int)
--gmcToNd message = if (T.head.(!!1).head.textTolistlisttext.dmTotext) message == '-' then  (textothpart message, numpart message) else (texttwopart message, 1)

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

--remindCheck :: (T.Text -> PostData -> [GetMessageCreate] -> IO T.Text) ->  PostData -> IO PostData
--remindCheck ptfunc postdata = do
-- today <- zonedTimeToLocalTime<$>getZonedTime
-- case divMod ((todHour.localTimeOfDay) today) 12 of
--  (0,_)  -> return postdata { noon = False }
--  (1,0)  -> getDirectoryContents reminddir >>= (\fs -> loop postdata today ptfunc (map (reminddir++) fs))
--  (1,_)  -> return postdata { noon = True }
--  where
--   loop :: PostData -> LocalTime -> (T.Text -> PostData -> [GetMessageCreate] -> IO T.Text) -> [FilePath] -> IO PostData
--   loop pd td ptfunc file = if null file || noon pd then return pd { noon = True }
--    else (doesFileExist.head) file >>= 
--     (\check -> if not check then loop pd td ptfunc (tail file)
--     else ( do
--      (getWeek, time, text)<-(\f->((read.T.unpack.head) f
--                                  ,(head.tail) f
--                                  ,(T.unlines.tail.tail) f)).T.lines<$>TIO.readFile (head file)
--      if dayToWeek td /= ((toEnum :: Int -> Week) getWeek) then loop pd td ptfunc (tail file)
--      else ( do
--       (ft,lt) <- getNum ':' time (25,61)
--       case makeTimeOfDayValid ft lt 0 of
--        Nothing  -> loop pd td ptfunc (tail file)
--        Just tod -> ( do
--         postid_str <- ptfunc text pd []
--         ctz <- getCurrentTimeZone
--         if T.null postid_str then loop pd td ptfunc (tail file)
--         else loop pd{schedule = (postid_str, ZonedTime{zonedTimeZone = ctz, zonedTimeToLocalTime = td{localTimeOfDay = tod}}):(schedule pd)} 
--                   td ptfunc (tail file)))))

dayToWeek :: LocalTime -> Week
dayToWeek day = do
 let [y,m,d] = ((\(a,b,c) -> [a+(div(toInteger b+12)15)-1,(toInteger b+12-12*div(12+toInteger b)15),toInteger c]).toGregorian.localDay) day
 ((toEnum :: Int -> Week).fromIntegral) (mod(d+div(26*(m+1))10+mod y 100+div(mod y 100)4+5*div y 100+div(div y 100)4+5)7)
