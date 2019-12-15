module Lib where

import TwitterAPI
import SlackAPI

import Control.Exception
import Control.Concurrent
import System.Directory
import System.IO
import Control.Exception
import Data.Time
import qualified Data.List as L
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Text.Lazy as TLazy
import qualified Data.Text.Lazy.IO as DTLIO
import qualified Data.Vector as V

-- have to MVector
data PostQueue = PostQueue { mentions :: V.Vector GetMention
                           , schedule :: V.Vector (T.Text, ZonedTime)-- twid, retweet time
                           , pqGroups :: V.Vector (T.Text, V.Vector T.Text)
                           } deriving (Show)

data Week = Monday
          | Tuesday
          | Wednesday
          | Thursday
          | Friday
          | Saturday
          | Sunday
          deriving (Show, Enum, Eq)

data Postfunc = Postfunc { tl       :: T.Text -> T.Text -> [String] -> IO(T.Text)
                         , dm       :: T.Text -> T.Text -> [String] -> IO(T.Text)}

getUserAllNULL = User { gur_id_str = T.empty
                      , gur_screen_name = T.empty}

getTLAllNULL = GetTL { gtl_text = T.empty
                     , gtl_id_str = T.empty
                     , gtl_in_reply_to_status_id_str = Nothing
                     , gtl_user = getUserAllNULL }

botscreen_name = "flow_6852"
 
--calcwebdir = "/home/share/posts/posts-available/"
--srvcalcdir = "/srv/calc-web/posts"
--reminddir = "/usr/local/calc-tweet/reminder/"
helpFile = "/usr/local/calc-tweet/helps/help.txt" 
twHelpFile = "/usr/local/calc-tweet/helps/tweet_help.txt"
uHelpFile = "/usr/local/calc-tweet/helps/users_help.txt"
gHelpFile = "/usr/local/calc-tweet/helps/groups_help.txt"
groupsconf = "/usr/local/calc-tweet/groups.conf"

emptyint = 1*1000*1000  :: Int {- 1 second -} 
mentiont = 12*1000*1000 :: Int {-12 second -}

searchReplyTree :: GetTL -> V.Vector GetTL -> T.Text -- create all message
searchReplyTree tl tls = if T.null (gtl_id_str tl) then T.empty else do
 let replys = V.filter ((==gtl_id_str tl).rpStatus) tls
     (seds, next) = sedsNext replys
 T.append (restoreSeds (gtl_text tl) (V.map gtl_text seds)) $ T.append (T.singleton '\n') $ searchReplyTree next tls
  where
   sedsNext :: V.Vector GetTL -> (V.Vector GetTL, GetTL)
   sedsNext rep = case V.find (not.isEqStrText "calc-tweet".(`getTlToCmd` 0).gtl_text) rep of
    Nothing -> (V.filter (isEqStrText "calc-tweet".(`getTlToCmd` 0).gtl_text) rep, getTLAllNULL)
    Just a  -> (V.filter (isEqStrText "calc-tweet".(`getTlToCmd` 0).gtl_text) rep, a)
   restoreSeds :: T.Text -> V.Vector T.Text -> T.Text
   restoreSeds text sed = case V.null sed of
    True  -> text
    False -> restoreSeds (T.replace (getTlToCmd (V.head sed) 2) (getTlToCmd (V.head sed) 3) text) (V.tail sed)


searchReplyId :: T.Text -> V.Vector GetTL -> [T.Text]
searchReplyId id tl = if T.null id then [] else
 ((\t -> case t of Nothing -> []
                   Just a  -> (gtl_id_str a):(searchReplyId (gtl_id_str a) tl)).V.find ((==id).rpStatus)) tl

rpStatus :: GetTL -> T.Text
rpStatus gtl = case gtl_in_reply_to_status_id_str gtl of
 Nothing -> T.empty
 Just x  -> x

getTweetNextId :: PostQueue -> T.Text
getTweetNextId msg = (T.pack.(show :: Integer -> String).(+(-1)).(read :: String -> Integer).T.unpack) $ getTweetId msg

getTweetId :: PostQueue -> T.Text
getTweetId msg = (snd.T.breakOnEnd (T.singleton '/')) $ (gul_expanded_url.Prelude.head.gen_urls.gmt_entities.V.head.mentions) msg

--scrapingId :: T.Text -> T.Text
--scrapingId text = if T.null text then T.empty else 
--                  if T.head text == '/' then (scrapingId.T.tail) text else T.append ((T.singleton.T.head) text) ((scrapingId.T.tail) text)

rmDup :: [T.Text] -> [T.Text]
rmDup = foldl (\seen x -> if x `elem` seen then seen else x:seen) []

filterCmd vmsgq n = ((!! n).Prelude.head.Prelude.map T.words.T.lines.gmt_text.V.head.mentions) vmsgq 

getTlToCmd msg n = ((!! n).Prelude.head.Prelude.map T.words.T.lines) msg

isEqStrText :: String -> T.Text -> Bool
isEqStrText str tx = case (Prelude.null str, T.null tx) of
 (True, True)   -> True
 (False, False) -> if((Prelude.head str) == (T.head tx)) then isEqStrText (Prelude.tail str) (T.tail tx) else False
 otherwise      -> False

-- getPermitUser :: T.Text -> Int -> IO Bool
-- getPermitUser uid n = if n < 0 || 4 < n then error "getPermitUser :: error" else do
--  users <- Prelude.map splitChar.T.lines <$> TIO.readFile usersconf
--  return $ ((\p -> case p of Nothing        -> False
--                             Just (a,b,c,d) -> case n of
--                                                    0 -> True -- change "me"
--                                                    1 -> b -- tweet
--                                                    2 -> c -- sudo
--                                                    3 -> d -- broadcast
--                                                    _ -> False).L.find (\(a,b,c,d) -> uid == a)) users

-- broadUsers :: IO T.Text
-- broadUsers = do
--  users <- Prelude.map splitChar.T.lines <$> TIO.readFile usersconf
--  return $ T.append (T.pack "@") ((T.intercalate (T.pack "\n@").Prelude.map (\(a,b,c,d) -> a).Prelude.filter (\(a,b,c,d) -> d)) users)
--   
-- 
-- permitRewrite :: T.Text -> (T.Text,Bool) -> (T.Text, Bool, Bool, Bool) -> (T.Text, Bool, Bool, Bool)
-- permitRewrite id (cmd,permit) (a,b,c,d) = if a /= id then (a,b,c,d) else case T.unpack cmd of
--  "post"      -> (a, permit, c, d)
--  "sudo"      -> (a, b, permit, d)
--  "broadcast" -> (a, b, c, permit)
--  _           -> (a,b,c,d)

splitChar :: T.Text -> (T.Text, Bool, Bool, Bool)
splitChar = ((\[a,b,c,d] -> (a,strToBool b,strToBool c,strToBool d)).commaSep) 

commaSep :: T.Text -> [T.Text]
-- commaSep text = if T.null text then [] else T.takeWhile (/=',') text:commaSep ((T.tail.(T.dropWhile (/=','))) text)
commaSep text = T.split (==',') text


commaIns :: [T.Text] -> T.Text
commaIns = T.intercalate (T.singleton ',')

strToBool str = if str == T.pack "True" then True else False

userInGroups :: T.Text -> V.Vector (T.Text, V.Vector T.Text) -> T.Text
userInGroups user raw = commaIns.V.toList.V.map fst.V.filter (isInGroup user) $ raw
 where
  isInGroup :: T.Text -> (T.Text, V.Vector T.Text) -> Bool
  isInGroup u g = case V.find (==u) (snd g) of
   Nothing -> False
   Just a  -> True

groupAndUsers :: T.Text -> V.Vector (T.Text, V.Vector T.Text) -> (T.Text, V.Vector T.Text)
groupAndUsers group raw = do
--  all <- V.fromList.Prelude.map (V.fromList.commaSep).T.lines.TLazy.toStrict <$> DTLIO.hGetContents h
 case V.find ((==group).fst) $ raw of
  Nothing -> (T.empty, V.empty)
  Just gr -> gr

userInGroup :: T.Text -> V.Vector (T.Text, V.Vector T.Text) -> (V.Vector T.Text)
userInGroup group raw = snd $ groupAndUsers group raw

allUsers :: V.Vector (T.Text, V.Vector T.Text) -> V.Vector T.Text
allUsers = userInGroup (T.pack "all")
-- TIO.readFile groupsconf >>= return.V.fromList.L.nub.Prelude.concat.Prelude.map (Prelude.tail.commaSep).T.lines

existInGroup :: T.Text -> T.Text -> V.Vector (T.Text, V.Vector T.Text) -> Bool
existInGroup user group raw = do
 let us = userInGroup group raw 
 case V.find (== user) us of
  Nothing -> False
  Just u  -> True

existUser :: T.Text -> V.Vector (T.Text, V.Vector T.Text) -> Bool
existUser user raw = do
 let all = allUsers raw
 case V.find (== user) all of
  Nothing -> False
  Just u  -> True

addUserInGroup :: T.Text -> T.Text -> V.Vector (T.Text, V.Vector T.Text) -> V.Vector (T.Text, V.Vector T.Text)
addUserInGroup user group raw = case V.elemIndex group ((V.map Prelude.fst) raw) of
 Nothing -> raw
 Just a  -> do
  let fraw = fst.V.splitAt a $ raw
      mraw = snd $ raw V.! a
      lraw = V.tail.snd.V.splitAt a $ raw
  case V.elem user mraw of
   True  -> raw
   False -> fraw V.++ (V.singleton (fst (raw V.! a),V.snoc mraw user)) V.++ lraw

rmUserInGroup :: T.Text -> T.Text -> V.Vector (T.Text, V.Vector T.Text) -> V.Vector (T.Text, V.Vector T.Text)
rmUserInGroup user group raw = case V.elemIndex group ((V.map Prelude.fst) raw) of
  Nothing -> raw
  Just a  -> do
   let fraw = fst.V.splitAt a $ raw
       mraw = V.filter (/=user) $ snd $ raw V.! a
       lraw = V.tail.snd.V.splitAt a $ raw
   fraw V.++ (V.singleton (fst (raw V.! a), mraw)) V.++ lraw

queueToUser :: PostQueue -> T.Text
queueToUser = gur_screen_name.gmt_user.V.head.mentions

addGroup :: T.Text -> V.Vector (T.Text, V.Vector T.Text) -> V.Vector (T.Text, V.Vector T.Text)
addGroup group raw = case V.elemIndex group ((V.map Prelude.fst) raw) of
 Just a  -> raw
 Nothing -> V.snoc raw (group, V.empty)

deleteGroup :: T.Text -> V.Vector (T.Text, V.Vector T.Text) -> V.Vector (T.Text, V.Vector T.Text)
deleteGroup group raw = case V.elemIndex group ((V.map Prelude.fst) raw) of
 Nothing -> raw
 Just a  -> V.filter ((/=group).fst) raw

-- before refuctoring, all comment out below
-- postTweet :: T.Text -> PostQueue -> T.Text -> [String] -> IO() 
-- postTweet usermsg postq postTarget botconf = if T.null postTarget then return () else do
--  let msgs = splitN postTarget 140 -- 140 is twitter max size
--  start_id <- showTL $ mkShowmsg (Prelude.head msgs) (T.empty) botconf -- Timeline
--  postTweetInReply (Prelude.tail msgs) start_id
--  showTL $ mkShowmsg usermsg ((gmid_str.V.head.mentions) postq) botconf
--  return ()
--  where
--   splitN :: T.Text -> Int -> [T.Text] 
--   splitN raw max = if T.length raw <= max then [raw] else (T.take max raw):splitN (T.drop max raw) max
--   postTweetInReply :: [T.Text] -> T.Text -> IO()
--   postTweetInReply [] _         = return ()
--   postTweetInReply (x:xs) nowid = if T.null nowid then return () else do
--    nextid <- showTL $ mkShowmsg x nowid botconf
--    postTweetInReply xs nextid
--   mkShowmsg :: T.Text -> T.Text -> [String] -> Showmsg
--   mkShowmsg text id conf = Showmsg {showtext = text, showid = id, showconf = conf}
-- 
-- monitoring :: MVar PostQueue -> T.Text -> [String] -> IO ()
-- monitoring msgq since_id botconf = do
--  threadDelay mentiont
--  --(rtCheck pd >>= remindCheck typeTL)-- monitoring retweeting
--  tlmention <- (\t -> case t of Left  e -> error e
--                                Right l -> (V.reverse.V.fromList) l) <$> getMention since_id botconf
--  puser <- V.fromList.Prelude.map splitChar.T.lines <$> TIO.readFile usersconf
--  if V.null tlmention then monitoring msgq since_id botconf
--  else do
--   nowq <- readMVar msgq 
--   (if (V.null.mentions) nowq then do
--    putMVar msgq nowq {mentions = V.filter (\x -> filterUserElem x puser && filterCmdCalcTweet x) tlmention}
--    forkIO $ cmdCheck msgq botconf
--    return ()
--   else do
--    befq <- takeMVar msgq 
--    putMVar msgq befq{mentions = ((mentions befq) V.++ (V.filter (\x -> filterUserElem x puser && filterCmdCalcTweet x) tlmention))}
--    ) >> monitoring msgq ((gmid_str.V.head) tlmention) botconf
--  where
--   filterUserElem :: GetMention -> V.Vector (T.Text, Bool, Bool, Bool) -> Bool
--   filterUserElem x   = V.elem ((gid_str.gmuser) x).V.map (\(a,b,c,d) -> a)
--   filterCmdCalcTweet = (=="calc-tweet").T.unpack.Prelude.head.T.words.gmtext
-- 
-- cmdCheck :: MVar PostQueue -> [String] -> IO ()
-- cmdCheck msgq botconf = readMVar msgq >>= \nowq -> if (V.null.mentions) nowq then return () else 
--  do
--   sc <- (case T.unpack (filterCmd nowq 1) of
--               "tweet" -> tweetCmd 
--               "user"  -> userCmd
-- --            "group" -> groupCmd // comming soon
-- --            "web"   -> webCmd
--               _       -> errorCmd) nowq botconf
--   addDeleteSchedule msgq sc  -- add or deleteschedule 
--   threadDelay cmdt
--   cmdCheck msgq botconf
--    where
--     addDeleteSchedule q d = takeMVar msgq >>= \x -> putMVar msgq x{ mentions = (V.tail.mentions)x
--                                                                   , schedule =  if V.null d then schedule x else schedule x V.++ d} 
--     cmdt = 60*1000*1000 -- 1min
-- 
-- -- tweet command 
-- tweetCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime))
-- tweetCmd msg botconf = (case T.unpack (filterCmd msg 2) of
--  "post"      -> twpostCmd
--  "broadcast" -> twbroadcastCmd 
--  "rm"        -> twrmCmd 
--  "help"      -> twHelpCmd
-- -- "set"       -> twsetCmd 
--  _           -> errorCmd) msg botconf
-- 
-- twpostCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime))
-- twpostCmd msg botconf = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 1 >>= \x -> if not x then return V.empty else do
--  let user_id  = (gid_str.gmuser.V.head.mentions) msg    -- twitter api name
--      since_id = getTweetId msg -- twitter api name 
--  userTL <- (\t -> case t of  Left e  -> error e
--                              Right l -> (V.reverse.V.fromList) l) <$> getUserTL user_id since_id botconf
--  let postmsg    = T.pack $ '@':(T.unpack.gid_str.gmuser.V.head.mentions) msg ++ "post done."
--      postTarget = searchReplyTree since_id userTL -- search and sed
--  postTweet postmsg msg postTarget botconf
--  return V.empty
-- 
-- twbroadcastCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime))
-- twbroadcastCmd msg botconf = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 1 >>= \x -> if not x then return V.empty else do
--  let user_id  = (gid_str.gmuser.V.head.mentions) msg    -- twitter api name
--      since_id = getTweetId msg -- twitter api name
--      postmsg  = T.pack $ '@':(T.unpack.gid_str.gmuser.V.head.mentions) msg ++ "post done."
--  userTL <- (\t -> case t of  Left e  -> error e
--                              Right l -> (V.reverse.V.fromList) l) <$> getUserTL user_id since_id botconf
--  broadTarget <- broadUsers 
--  let postTarget = T.append broadTarget (searchReplyTree since_id userTL)-- search and sed
--  postTweet postmsg msg postTarget botconf
--  return V.empty
-- 
-- twrmCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime))
-- twrmCmd msg botconf = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 1 >>= \x -> if not x then return V.empty else do
--  let user_id  = (gid_str.gmuser.V.head.mentions) msg -- twitter api name
--      since_id = getTweetId msg -- twitter api name
--  userTL <- (\t -> case t of  Left e  -> error e
--                              Right l -> (V.reverse.V.fromList) l) <$> getUserTL (T.pack "calc-mie") since_id botconf
--  let postTarget = searchReplyId since_id userTL
--      postmsg    = T.pack $ '@':(T.unpack.gid_str.gmuser.V.head.mentions) msg ++ "remove done."
--  rmTweets postmsg msg postTarget botconf
--  return V.empty
-- 
-- twHelpCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime))
-- twHelpCmd msg botconf = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 0 >>= \x -> if not x then return V.empty else do
--  let postTarget = T.empty
--  postmsg <- T.append (T.pack ('@':((T.unpack.gid_str.gmuser.V.head.mentions) msg))) <$> TIO.readFile twHelpFile
--  postTweet postmsg msg postTarget botconf
--  return V.empty
-- 
-- -- comming soon?
-- --twsetCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime))
-- --twsetCmd msg botconf = do
-- 
-- searchReplyTree :: T.Text -> V.Vector Tweet ->  T.Text -- create all message
-- searchReplyTree id tl = if T.null id then T.empty else 
--  ((\t -> case t of Nothing -> T.empty 
--                    Just a  -> T.append (text a) (searchReplyTree (id_str a) tl)).V.find (\x -> ((==id).gtl_in_reply_to_status_id_str) x)) tl
-- 
-- searchReplyId :: T.Text -> V.Vector Tweet -> [T.Text]
-- searchReplyId id tl = if T.null id then [] else
--  ((\t -> case t of Nothing -> []
--                    Just a  -> (id_str a):(searchReplyId (id_str a) tl)).V.find (\x -> ((==id).gtl_in_reply_to_status_id_str) x)) tl
-- 
-- getTweetId :: PostQueue -> T.Text
-- getTweetId msg = T.pack $ (((show :: Integer -> String).(+(-1)).(read :: String -> Integer).T.unpack.scrapingId) (filterCmd msg 3))
-- 
-- scrapingId :: T.Text -> T.Text
-- scrapingId text = if T.null text then T.empty else 
--                   if T.head text == '/' then (scrapingId.T.tail) text else T.append ((T.singleton.T.head) text) ((scrapingId.T.tail) text)
-- 
-- 
-- rmTweets :: T.Text -> PostQueue -> [T.Text] -> [String] -> IO()
-- rmTweets postmsg postq twid botconf = 
--  if Prelude.null twid then tweet postmsg ((gmid_str.V.head.mentions) postq) botconf >> return () 
--  else rmTweet (Prelude.head twid) botconf >> rmTweets postmsg postq (Prelude.tail twid) botconf
-- 
-- -- user command
-- userCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime))
-- userCmd msg botconf = (case T.unpack (filterCmd msg 2) of
--  "add"  -> uaddCmd
--  "rm"   -> urmCmd 
--  "set"  -> usetCmd
--  "help" -> uhelpCmd
--  _      -> errorCmd) msg botconf
-- 
-- uaddCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime))
-- uaddCmd msg botconf = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 2 >>= \x -> if not x then return V.empty else do
--  let user_id    = (gid_str.gmuser.V.head.mentions) msg
--      postTarget = T.pack $ "done add user" ++ T.unpack (filterCmd msg 3)
--      users      = (rmDup.Prelude.drop 3.T.words.gmtext.V.head.mentions) msg
--  pusers <- Prelude.map splitChar.T.lines <$> TIO.readFile usersconf
--  return V.empty
-- 
-- urmCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime))
-- urmCmd msg botconf = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 2 >>= \x -> if not x then return V.empty else do
--  let user_id    = (gid_str.gmuser.V.head.mentions) msg
--      postmsg    = T.pack $ "done rm user" ++ T.unpack (filterCmd msg 3)
--      users      = (rmDup.Prelude.drop 3.T.words.gmtext.V.head.mentions) msg
--      postTarget = T.empty
--  pusers <- Prelude.map splitChar.T.lines <$> TIO.readFile usersconf
--  TIO.writeFile usersconf ((T.unlines.Prelude.map tupleToString) (rmUserCmd users pusers))
--  postTweet postmsg msg postTarget botconf
--  return V.empty
--   where
--    rmUserCmd u p = if Prelude.null p then [] else 
--                    if (((\(a,b,c,d) -> a).head) p) `elem` u then rmUserCmd u (Prelude.tail p) else
--                    (head p):rmUserCmd u (Prelude.tail p)
--    tupleToString (a,b,c,d) = T.pack $ (T.unpack a) ++ ',':boolToStr b ++ ',':boolToStr c ++ ',':boolToStr d
--    boolToStr True  = "True"
--    boolToStr False = "False"
--  
-- usetCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime))
-- usetCmd msg botconf = do -- calc-tweet user set cmd user permit
--  let cmd      = filterCmd msg 3 -- cmd = post, sudo, broadcast
--      user     = filterCmd msg 4 -- user is target
--      permit   = filterCmd msg 5 -- permit = True or False
--      sudoflug = if T.unpack cmd == "sudo" || (T.unpack cmd /= "sudo" && T.unpack user /= "me") then 2 else 0 -- have to sudo or me
--  getPermitUser ((gid_str.gmuser.V.head.mentions)msg) sudoflug >>= \x -> if not x then return V.empty else do
--   users <- Prelude.map (permitRewrite user (cmd,strToBool permit).splitChar).T.lines <$> TIO.readFile usersconf
--   return V.empty
--   
-- uhelpCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime))
-- uhelpCmd msg botconf = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 0 >>= \x -> if not x then return V.empty else do
--  let user_id    = (gid_str.gmuser.V.head.mentions) msg
--      postTarget = T.empty
--  postmsg <- TIO.readFile uHelpFile
--  postTweet postmsg msg postTarget botconf
--  return V.empty
-- 
-- -- web command comming soon?
-- --webCmd :: MVar PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime)) -- post web
-- --webCmd msg botconf postdata = calcWebPost postdata msg botconf typeTL
-- 
-- --groupCmd :: PostQueue -> [String] -> (T.Text -> T.Text -> [String] -> IO(T.Text)) -> IO (V.Vector (T.Text, ZonedTime))
-- --groupCmd msg botconf func = (case T.unpack (filterCmd msg 2) of
-- -- "create" -> gcreateCmd
-- -- "add"    -> gaddCmd
-- -- "rm"     -> grmCmd 
-- -- "delete" -> gdeleteCmd
-- -- "show"   -> gshowCmd
-- -- "help"   -> ghelpCmd
-- -- _        -> errorCmd) msg botconf func
-- --
-- ---- calc-tweet group create name
-- --gcreateCmd :: PostQueue -> [String] -> (T.Text -> T.Text -> [String] -> IO(T.Text)) -> IO (V.Vector (T.Text, ZonedTime))
-- --gcreateCmd msg botconf func = getPermitUser((gid_str.gmuser.V.head.mentions)msg) 2 >>= \x -> if not x then return V.empty else do
-- --
-- ---- calc-tweet group add group user
-- --gaddCmd :: PostQueue -> [String] -> (T.Text -> T.Text -> [String] -> IO(T.Text)) -> IO (V.Vector (T.Text, ZonedTime))
-- --gaddCmd msg botconf func = getPermitUser((gid_str.gmuser.V.head.mentions)msg) 2 >>= \x -> if not x then return V.empty else do
-- --
-- ---- calc-tweet group rm group user
-- --grmCmd :: PostQueue -> [String] -> (T.Text -> T.Text -> [String] -> IO(T.Text)) -> IO (V.Vector (T.Text, ZonedTime))
-- --grmCmd msg botconf func = getPermitUser((gid_str.gmuser.V.head.mentions)msg) 2 >>= \x -> if not x then return V.empty else do
-- --
-- ---- calc-tweet group delete name
-- --gdeleteCmd :: PostQueue -> [String] -> (T.Text -> T.Text -> [String] -> IO(T.Text)) -> IO (V.Vector (T.Text, ZonedTime))
-- --gdeleteCmd msg botconf func = getPermitUser((gid_str.gmuser.V.head.mentions)msg) 2 >>= \x -> if not x then return V.empty else do
-- --
-- ---- calc-tweet group show name post DM 
-- --gshowCmd :: PostQueue -> [String] -> (T.Text -> T.Text -> [String] -> IO(T.Text)) -> IO (V.Vector (T.Text, ZonedTime))
-- --gshowCmd msg botconf func = getPermitUser((gid_str.gmuser.V.head.mentions)msg) 2 >>= \x -> if not x then return V.empty else do
-- --
-- ---- calc-tweet group help
-- --ghelpCmd :: PostQueue -> [String] -> (T.Text -> T.Text -> [String] -> IO(T.Text)) -> IO (V.Vector (T.Text, ZonedTime))
-- --ghelpCmd msg botconf func = getPermitUser((gid_str.gmuser.V.head.mentions)msg) 0 >>= \x -> if not x then return V.empty else do
-- 
-- -- command error post
-- errorCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime)) -- post error
-- errorCmd msg botconf = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 0 >>= \x -> if not x then return V.empty else do
--  let postmsg    = T.pack "command error..."
--      postTarget = T.empty
--  postTweet postmsg msg postTarget botconf
--  return V.empty
-- 
-- rmDup :: [T.Text] -> [T.Text]
-- rmDup = foldl (\seen x -> if x `elem` seen then seen else x:seen) []
-- 
-- filterCmd vmsgq n = ((!! n).Prelude.head.Prelude.map T.words.T.lines.gmtext.V.head.mentions) vmsgq 
-- 
-- getPermitUser :: T.Text -> Int -> IO Bool
-- getPermitUser uid n = if n < 0 || 4 < n then error "getPermitUser :: error" else do
--  users <- Prelude.map splitChar.T.lines <$> TIO.readFile usersconf
--  return $ ((\p -> case p of Nothing        -> False
--                             Just (a,b,c,d) -> case n of
--                                                    0 -> True -- change "me"
--                                                    1 -> b -- tweet
--                                                    2 -> c -- sudo
--                                                    3 -> d -- broadcast
--                                                    _ -> False).L.find (\(a,b,c,d) -> uid == a)) users
-- 
-- broadUsers :: IO T.Text
-- broadUsers = do
--  users <- Prelude.map splitChar.T.lines <$> TIO.readFile usersconf
--  return $ T.append (T.pack "@") ((T.intercalate (T.pack "\n@").Prelude.map (\(a,b,c,d) -> a).Prelude.filter (\(a,b,c,d) -> d)) users)
--   
-- 
-- permitRewrite :: T.Text -> (T.Text,Bool) -> (T.Text, Bool, Bool, Bool) -> (T.Text, Bool, Bool, Bool)
-- permitRewrite id (cmd,permit) (a,b,c,d) = if a /= id then (a,b,c,d) else case T.unpack cmd of
--  "post"      -> (a, permit, c, d)
--  "sudo"      -> (a, b, permit, d)
--  "broadcast" -> (a, b, c, permit)
--  _           -> (a,b,c,d)
-- 
-- splitChar :: T.Text -> (T.Text, Bool, Bool, Bool)
-- splitChar = ((\[a,b,c,d] -> (a,strToBool b,strToBool c,strToBool d)).commaSep) 
--  where
--   commaSep :: T.Text -> [T.Text]
--   commaSep text = if T.null text then [] else T.takeWhile (/=',') text:commaSep ((T.tail.(T.dropWhile (/=','))) text)
-- 
-- strToBool str = if str == T.pack "True" then True else False
-- 
-- --showTL :: T.Text -> T.Text -> [String] -> IO(T.Text) 
-- --showTL msg id botconf =  (\t -> case t of Left  e -> error e
-- --                                          Right t -> post_tl_id_str t) <$>tweet msg id botconf 
-- --
-- --showTerm :: T.Text -> T.Text -> [String] -> IO(T.Text)
-- --showTerm msg id botconf = print msg >> print id >> return T.empty
-- 
-- --postTweet :: PostData -> [GetMessageCreate] -> (T.Text -> PostData -> [GetMessageCreate] -> IO T.Text) -> IO PostData
-- --postTweet postdata tw ptfunc= do 
-- -- let ntdata = createNoticeData (takeWhile (((\x -> (x/=(T.pack "$clear")) && (x/=(T.pack "$post"))).head.head.textTolistlisttext.dmTotext)) tw)
-- --                               NoticeData{notice = T.pack "", date = [], time = [], locale =[]}
-- -- if (T.null.notice) ntdata then return postdata
-- -- else (do
-- --  posttw <- TIO.readFile noticetempconf
-- --  let posttx = makeTweet ntdata 1 ((maximum.map (maximum.map snd.((T.pack "null",0):)))[date ntdata, time ntdata, locale ntdata]) 
-- --                                                                (T.append posttw (T.append (notice ntdata) (T.pack "\n")))
-- --  postid_str <- ptfunc posttx postdata tw
-- --  if T.null postid_str then return postdata
-- --  else setNoticeTime  postdata ntdata postid_str >>= (\r->return postdata {schedule = r}) )
-- 
-- --calcWebPost :: PostData -> GetMention -> [String] -> (T.Text -> PostData -> [GetMessageCreate] -> IO T.Text) -> IO PostData
-- --calcWebPost postdata tw botconf ptfunc= do
-- -- nowpost <- getDirectoryContents srvcalcdir
-- -- let newarticle = filter (\x->x `notElem` (calcweb postdata)) nowpost
-- -- if null newarticle then return postdata
-- -- else loop newarticle nowpost botconf
-- --  where
-- --   loop :: [String] -> [String] -> [String] -> IO PostData
-- --   loop na np conf =
-- --    if null na then return postdata { calcweb = np }
-- --    else ( do
-- --      article <- T.lines<$>TIO.readFile (calcwebdir ++ ((takeWhile (/= '.')).head) na ++ ".md")
-- --      let title  = T.drop 7 (article!!1)
-- --          author = (article!!2)
-- --          webtx  = T.pack $ T.unpack author ++ "\n" 
-- --                          ++ T.unpack title ++ "について書きました。\n url: https://calc.mie.jp/posts/" 
-- --                          ++ head na
-- --      ptfunc webtx postdata tw conf
-- --      loop (tail na) np )
--   
-- --createNoticeData :: [GetMessageCreate] -> NoticeData -> NoticeData
-- --createNoticeData messages ntdata = if null messages || noticeAll ntdata then ntdata else createNoticeData (tail messages) (
-- --  case (T.unpack.parampart.head) messages of
-- --   "$notice" -> if (T.null.notice) ntdata then ntdata{notice = (texttwopart.head) messages} else ntdata
-- --   "$date"   -> if ((snd.gmcToNd.head) messages) `elem` (map snd (date ntdata)) then ntdata else ntdata { date = ((gmcToNd.head) messages):date ntdata }
-- --   "$time"   -> if ((snd.gmcToNd.head) messages) `elem` (map snd (time ntdata)) then ntdata else ntdata { time = ((gmcToNd.head) messages):time ntdata }
-- --   "$locale" -> if ((snd.gmcToNd.head) messages) `elem` (map snd (locale ntdata)) then ntdata else ntdata { locale = ((gmcToNd.head) messages):locale ntdata }
-- --   _         -> ntdata
-- --  )
-- 
-- 
-- --gmcToNd :: GetMessageCreate -> (T.Text,Int)
-- --gmcToNd message = if (T.head.(!!1).head.textTolistlisttext.dmTotext) message == '-' then  (textothpart message, numpart message) else (texttwopart message, 1)
-- 
-- --noticeAll :: NoticeData -> Bool
-- --noticeAll ntdata = if (T.null.notice) ntdata || (null.date) ntdata || (null.time) ntdata || (null.locale) ntdata then False else True
--  
-- --makeTweet :: NoticeData -> Int -> Int -> T.Text -> T.Text
-- --makeTweet ntdata n mx tw = if n>mx then tw else 
-- -- makeTweet ntdata (n+1) mx (if n `elem` concatMap (map snd) [date ntdata, time ntdata, locale ntdata] then
-- --                             T.append tw (
-- --                             T.append (elemText n (date ntdata)) (
-- --                             T.append (elemText n (time ntdata)) (
-- --                             T.append (if (T.null.elemText n) (locale ntdata) then T.pack "" 
-- --                                                else T.append (T.pack "＠.") (elemText n (locale ntdata))) (T.pack "\n"))))  -- createSchedule
-- --                             else tw)
-- 
-- --elemText :: Int -> [(T.Text, Int)] -> T.Text
-- --elemText n text = if (n `notElem` map snd text) then T.pack "" 
-- --                        else T.append ((fst.head.filter ((==n).snd)) text) (T.pack " ")
-- 
-- --userAdd :: PostData -> [GetMessageCreate] -> IO PostData
-- --userAdd postdata tw = do
-- -- let user = ((T.drop 9.gettext.getmessage_data.getmessage_create.head) tw)
-- -- permituser <- T.lines<$>TIO.readFile usersconf
-- -- if user `notElem` permituser then (TIO.appendFile usersconf user >> return postdata)
-- -- else return postdata 
-- 
-- --setNoticeTime :: PostData -> NoticeData -> T.Text -> IO [(T.Text,ZonedTime)]
-- --setNoticeTime pdt ndt res = loop 1 ((maximum.map (maximum.map snd.((T.pack "null",0):)))[date ndt, time ndt]) [date ndt, time ndt] pdt
-- -- where 
-- --   loop :: Int -> Int -> [[(T.Text,Int)]] -> PostData -> IO[(T.Text,ZonedTime)]
-- --   loop n mx [dat, tim] pdt
-- --    | n>mx = (return.schedule) pdt 
-- --    | all (elem n.map snd) [dat, tim] = do
-- --       ctz     <- getCurrentTimeZone
-- --       (ft,lt) <- getNum ':' ((fst.head.filter ((==n).snd)) tim) (25,61)
-- --       (fd,ld) <- getNum '/' ((fst.head.filter ((==n).snd)) dat) (13,32)
-- --       year <- (\(y,m,d)->if fd < m then y+1 else y).toGregorian.localDay.zonedTimeToLocalTime<$>getZonedTime
-- --       case makeTimeOfDayValid ft lt 0 of
-- --        Nothing -> loop (n+1) mx [dat,tim] pdt 
-- --        Just t  -> case fromGregorianValid year fd ld of
-- --                    Nothing -> loop (n+1) mx [dat,tim] pdt
-- --                    Just d  -> do
-- --                     next <- loop (n+1) mx [dat,tim] pdt
-- --                     return ((res,ZonedTime { zonedTimeToLocalTime = LocalTime { localDay = d
-- --                                                                               , localTimeOfDay = t},
-- --                                              zonedTimeZone = ctz}):next) 
-- --    | otherwise =  loop (n+1) mx [dat, tim] pdt
-- --
-- --getNum :: Char -> T.Text -> (Int,Int) -> IO(Int,Int)
-- --getNum c text (f,l) = if T.null text then return (f,l) else case (elemIndex c.T.unpack.T.filter (\x->x `elem` (c:['0'..'9']))) text of
-- -- Nothing -> return (f,l)
-- -- Just n  -> catch(
-- --             (return.(\(a,b)->((read.T.unpack) a,(read.T.unpack.T.take 2.T.tail) b)).T.splitAt n.T.filter (\x->x `elem` (c:['0'..'9']))) text)
-- --             $ \(SomeException e) -> return (f,l)
-- 
-- --rtCheck :: PostData -> IO PostData
-- --rtCheck postdata = do
-- -- now <- zonedTimeToUTC<$>getZonedTime
-- -- let rtlist = filter ((<=15*60).((`diffUTCTime` now).zonedTimeToUTC.snd)) (schedule postdata)
-- -- case null rtlist of
-- --  True  -> return postdata 
-- --  False -> loop rtlist postdata
-- --   where 
-- --    loop rtl pdt = if null rtl then
-- --                    return (setPostData ( befts pdt
-- --                                        , calcweb pdt 
-- --                                        , filter ((>15*60).((`diffUTCTime` now).zonedTimeToUTC.snd)) (schedule pdt)
-- --                                        , noon pdt))
-- --                   else  
-- --                    (do
-- --                      postRT ((fst.head) rtl) 
-- --                      loop (tail rtl) pdt )
-- 
-- --remindCheck :: (T.Text -> PostData -> [GetMessageCreate] -> IO T.Text) ->  PostData -> IO PostData
-- --remindCheck ptfunc postdata = do
-- -- today <- zonedTimeToLocalTime<$>getZonedTime
-- -- case divMod ((todHour.localTimeOfDay) today) 12 of
-- --  (0,_)  -> return postdata { noon = False }
-- --  (1,0)  -> getDirectoryContents reminddir >>= (\fs -> loop postdata today ptfunc (map (reminddir++) fs))
-- --  (1,_)  -> return postdata { noon = True }
-- --  where
-- --   loop :: PostData -> LocalTime -> (T.Text -> PostData -> [GetMessageCreate] -> IO T.Text) -> [FilePath] -> IO PostData
-- --   loop pd td ptfunc file = if null file || noon pd then return pd { noon = True }
-- --    else (doesFileExist.head) file >>= 
-- --     (\check -> if not check then loop pd td ptfunc (tail file)
-- --     else ( do
-- --      (getWeek, time, text)<-(\f->((read.T.unpack.head) f
-- --                                  ,(head.tail) f
-- --                                  ,(T.unlines.tail.tail) f)).T.lines<$>TIO.readFile (head file)
-- --      if dayToWeek td /= ((toEnum :: Int -> Week) getWeek) then loop pd td ptfunc (tail file)
-- --      else ( do
-- --       (ft,lt) <- getNum ':' time (25,61)
-- --       case makeTimeOfDayValid ft lt 0 of
-- --        Nothing  -> loop pd td ptfunc (tail file)
-- --        Just tod -> ( do
-- --         postid_str <- ptfunc text pd []
-- --         ctz <- getCurrentTimeZone
-- --         if T.null postid_str then loop pd td ptfunc (tail file)
-- --         else loop pd{schedule = (postid_str, ZonedTime{zonedTimeZone = ctz, zonedTimeToLocalTime = td{localTimeOfDay = tod}}):(schedule pd)} 
-- --                   td ptfunc (tail file)))))
-- 
-- --dayToWeek :: LocalTime -> Week
-- --dayToWeek day = do
-- -- let [y,m,d] = ((\(a,b,c) -> [a+(div(toInteger b+12)15)-1,(toInteger b+12-12*div(12+toInteger b)15),toInteger c]).toGregorian.localDay) day
-- -- ((toEnum :: Int -> Week).fromIntegral) (mod(d+div(26*(m+1))10+mod y 100+div(mod y 100)4+5*div y 100+div(div y 100)4+5)7)
