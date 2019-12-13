module Exec where

import SlackAPI
import TwitterAPI
import Lib

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

-- post part
postTweet :: T.Text -> PostQueue -> T.Text -> [String] -> (T.Text -> T.Text -> [String] -> IO(T.Text)) -> IO() 
postTweet usermsg postq postTarget botconf func = if T.null postTarget then return () else do
 let msgs = splitN postTarget 140 -- 140 is twitter max size
 putStrLn "==== print msg ====="
 print msgs
 putStrLn "==== print msg ====="
 start_id <- func (Prelude.head msgs) (T.empty) botconf -- Timeline
 if (not.Prelude.null) msgs then postTweetInReply (Prelude.tail msgs) start_id func else do
  func usermsg ((gmt_id_str.V.head.mentions) postq) botconf
  return ()
 where
  splitN :: T.Text -> Int -> [T.Text] 
  splitN raw max = if T.length raw <= max then [raw] else (T.take max raw):splitN (T.drop max raw) max
  postTweetInReply :: [T.Text] -> T.Text -> (T.Text -> T.Text -> [String] -> IO(T.Text)) -> IO()
  postTweetInReply [] _ _            = return ()
  postTweetInReply (x:xs) nowid func = if T.null nowid then return () else do
   nextid <- func x nowid botconf
   postTweetInReply xs nextid func

-- tweet command part
twpostCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
twpostCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "post") (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> do
  let user_id  = (gur_id_str.gmt_user.V.head.mentions) msg    -- twitter api name
      since_id = getTweetId msg -- twitter api name 
  userTL <- (\t -> case t of  Left e  -> error e
                              Right l -> (V.reverse.V.fromList) l) <$> getUserTL user_id since_id botconf
  let postmsg    = T.pack $ '@':(T.unpack.gur_screen_name.gmt_user.V.head.mentions) msg ++ "post done."
      postTarget = searchReplyTree since_id userTL -- search and sed
  putStrLn "===== output ======"
  print userTL
  print postmsg
  print postTarget
  putStrLn "==================="
  postTweet postmsg msg postTarget botconf $ tl func
  return (V.empty, pqGroups msg)

-- twgroupCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime))
-- twgroupCmd msg botconf func =  existInGroup (queueToUser msg) (T.pack "post") >>= \x -> if not x then return V.empty else do
--  let user_id  = (gur_id_str.gmt_user.V.head.mentions) msg    -- twitter api name
--      since_id = getTweetId msg -- twitter api name
--      postmsg  = T.pack $ '@':(T.unpack.gur_id_str.gmt_user.V.head.mentions) msg ++ "post done."
--  userTL <- (\t -> case t of  Left e  -> error e
--                              Right l -> (V.reverse.V.fromList) l) <$> getUserTL user_id since_id botconf
--  broadTarget <- T.unwords.Prelude.map (\x-> T.pack '@':T.unpack x) <$> userInGroup (T.pack "broadcast")
--  let postTarget = T.append broadTarget (searchReplyTree since_id userTL)-- search and sed
--  postTweet postmsg msg postTarget botconf $ tl func
--  return V.empty

twrmCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
twrmCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "post") (pqGroups msg) of 
 False -> return (V.empty, pqGroups msg)
 True  -> do
  let user_id  = (gur_id_str.gmt_user.V.head.mentions) msg -- twitter api name
      since_id = getTweetId msg -- twitter api name
  userTL <- (\t -> case t of  Left e  -> error e
                              Right l -> (V.reverse.V.fromList) l) <$> getUserTL (T.pack "calc-mie") since_id botconf
  let postTarget = searchReplyId since_id userTL
      postmsg    = T.pack $ '@':(T.unpack.gur_id_str.gmt_user.V.head.mentions) msg ++ "remove done."
  rmTweets postmsg msg postTarget botconf
  return (V.empty, pqGroups msg)

twgroupCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
twgroupCmd msg botconf func = case (existInGroup (queueToUser msg) (T.pack "post")) (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> do
   let user_id  = (gur_id_str.gmt_user.V.head.mentions) msg    -- twitter api name
       since_id = getTweetId msg -- twitter api name
   userTL <- (\t -> case t of  Left e  -> error e
                               Right l -> (V.reverse.V.fromList) l) <$> getUserTL user_id since_id botconf
   let (group, users) = groupAndUsers (filterCmd msg 2) (pqGroups msg)
   let postmsg    = T.pack $ '@':(T.unpack.gur_id_str.gmt_user.V.head.mentions) msg ++ (if T.null group then "group is not exist..."
                                                                                                        else "post done.")
       postTarget = if T.null group then T.empty
                                    else T.append (replyUsers users) (searchReplyTree since_id userTL) -- search and sed
   postTweet postmsg msg postTarget botconf $ tl func
   return (V.empty, pqGroups msg)
    where
     replyUsers :: V.Vector T.Text -> T.Text
     replyUsers users = T.append (T.singleton '@') ((T.intercalate (T.pack " @").V.toList) users)

twHelpCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
twHelpCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "all") (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> do
  let postTarget = T.empty
  postmsg <- T.append (T.pack ('@':((T.unpack.gur_id_str.gmt_user.V.head.mentions) msg))) <$> TIO.readFile twHelpFile
  postTweet postmsg msg postTarget botconf $ dm func
  return (V.empty, pqGroups msg)

-- comming soon?
--twsetCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime))
--twsetCmd msg botconf = do

rmTweets :: T.Text -> PostQueue -> [T.Text] -> [String] -> IO()
rmTweets postmsg postq twid botconf = 
 if Prelude.null twid then tweet postmsg ((gmt_id_str.V.head.mentions) postq) botconf >> return () 
 else rmTweet (Prelude.head twid) botconf >> rmTweets postmsg postq (Prelude.tail twid) botconf

-- user command part
uaddCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
uaddCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "sudo") (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> do
  let user_id    = queueToUser msg
      postmsg    = T.empty
      postTarget = T.pack $ "add user" ++ T.unpack (filterCmd msg 3) ++ "done."
      users      = (rmDup.Prelude.drop 3.T.words.gmt_text.V.head.mentions) msg
  postTweet postmsg msg postTarget botconf $ dm func
  return (V.empty, addUserInGroup user_id (T.pack "all") (pqGroups msg))

urmCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
urmCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "sudo") (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> do
  let user_id    = queueToUser msg
      postmsg    = T.pack $ "rm user" ++ T.unpack (filterCmd msg 3) ++ "done"
      users      = (rmDup.Prelude.drop 3.T.words.gmt_text.V.head.mentions) msg
      postTarget = T.empty
  postTweet postmsg msg postTarget botconf $ dm func
  return (V.empty, rmUserInGroup user_id (T.pack "all") (pqGroups msg))
  
uhelpCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
uhelpCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "all") (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> do
  let user_id    = (gur_id_str.gmt_user.V.head.mentions) msg
      postTarget = T.empty
  postmsg <- TIO.readFile uHelpFile
  postTweet postmsg msg postTarget botconf $ dm func 
  return (V.empty, pqGroups msg)

-- group command part
-- calc-tweet group create name
gcreateCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
gcreateCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "sudo") (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> return (V.empty, addGroup (filterCmd msg 3) (pqGroups msg))

-- calc-tweet group add group user
gaddCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
gaddCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "sudo") (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> do
  let group   = filterCmd msg 3 
  let addUser = filterCmd msg 4
  return (V.empty, addUserInGroup addUser group (pqGroups msg))

-- calc-tweet group rm group user
grmCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
grmCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "sudo") (pqGroups msg) of 
 False -> return (V.empty, pqGroups msg)
 True  -> do
  let group  = filterCmd msg 3
      rmUser = filterCmd msg 4
  return (V.empty, rmUserInGroup rmUser group (pqGroups msg))

-- calc-tweet group delete name
gdeleteCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
gdeleteCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "sudo") (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> return (V.empty, deleteGroup (filterCmd msg 3) (pqGroups msg))

-- calc-tweet group show name post DM 
--gshowCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime))
--gshowCmd msg botconf func = getPermitUser((gid_str.gmuser.V.head.mentions)msg) 2 >>= \x -> if not x then return V.empty else do

-- calc-tweet group help
ghelpCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
ghelpCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "sudo") (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> do
  let user_id    = (gur_id_str.gmt_user.V.head.mentions) msg
      postTarget = T.empty
  postmsg <- TIO.readFile gHelpFile
  postTweet postmsg msg postTarget botconf $ dm func 
  return (V.empty, pqGroups msg)

-- error command part
errorCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text)) -- post error
errorCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "all") (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> do
  let postmsg    = T.pack "command error..."
      postTarget = T.empty
  postTweet postmsg msg postTarget botconf $ dm func
  return (V.empty, pqGroups msg)

