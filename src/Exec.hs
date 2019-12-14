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
postTweet :: PostQueue -> T.Text -> [String] -> (T.Text -> T.Text -> [String] -> IO(T.Text)) -> IO() 
postTweet postq postTarget botconf func = if T.null postTarget then return () else do
 let msgs = splitN postTarget 140 -- 140 is twitter max size
 putStrLn "==== print msg ====="
 print msgs
 putStrLn "==== print msg ====="
 start_id <- func (Prelude.head msgs) (T.empty) botconf -- Timeline
 if (not.Prelude.null) msgs then postTweetInReply (Prelude.tail msgs) start_id func else return ()
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
      since_id = getTweetNextId msg -- twitter api name 
  userTL <- (\t -> case t of  Left e  -> error e
                              Right l -> (V.reverse.V.fromList) l) <$> getUserTL user_id since_id botconf
  let firstGetTL = ((\t -> case t of Nothing -> getTLAllNULL
                                     Just t  -> t).V.find ((==(getTweetId msg)).gtl_id_str)) userTL
      postTarget = searchReplyTree firstGetTL userTL -- search and sed
  postTweet msg postTarget botconf $ tl func
  return (V.empty, pqGroups msg)

twrmCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
twrmCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "post") (pqGroups msg) of 
 False -> return (V.empty, pqGroups msg)
 True  -> do
  let user_id  = (gur_id_str.gmt_user.V.head.mentions) msg -- twitter api name
      since_id = getTweetId msg -- twitter api name
  userTL <- (\t -> case t of  Left e  -> error e
                              Right l -> (V.reverse.V.fromList) l) <$> getUserTL (T.pack "flow_6852") since_id botconf
  let postTarget = since_id:searchReplyId since_id userTL
  putStrLn "====== ids ====="
  print since_id
  print postTarget
  putStrLn "================"
  rmTweets msg postTarget botconf
  return (V.empty, pqGroups msg)

twgroupCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
twgroupCmd msg botconf func = case (existInGroup (queueToUser msg) (T.pack "post")) (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> do
   let user_id  = (gur_id_str.gmt_user.V.head.mentions) msg    -- twitter api name
       since_id = getTweetNextId msg -- twitter api name
   userTL <- (\t -> case t of  Left e  -> error e
                               Right l -> (V.reverse.V.fromList) l) <$> getUserTL user_id since_id botconf
   let firstGetTL = ((\t -> case t of Nothing -> getTLAllNULL
                                      Just t  -> t).V.find ((==(getTweetId msg)).gtl_id_str)) userTL
   let (group, users) = groupAndUsers (filterCmd msg 2) (pqGroups msg)
       postTarget = if T.null group then T.empty
                                    else T.append (replyUsers users) (searchReplyTree firstGetTL userTL) -- search and sed
   postTweet msg postTarget botconf $ tl func
   return (V.empty, pqGroups msg)
    where
     replyUsers :: V.Vector T.Text -> T.Text
     replyUsers users = T.append (T.singleton '@') ((T.intercalate (T.pack " @").V.toList) users)

twHelpCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
twHelpCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "all") (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> do
  postTarget <- T.append (T.pack ('@':((T.unpack.gur_id_str.gmt_user.V.head.mentions) msg))) <$> TIO.readFile twHelpFile
  postTweet msg postTarget botconf $ dm func
  return (V.empty, pqGroups msg)

-- comming soon?
--twsetCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime))
--twsetCmd msg botconf = do

rmTweets :: PostQueue -> [T.Text] -> [String] -> IO()
rmTweets postq twid botconf = 
 if Prelude.null twid then return () 
 else rmTweet (Prelude.head twid) botconf >>= print >> rmTweets postq (Prelude.tail twid) botconf

-- user command part
uaddCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
uaddCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "sudo") (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> do
  let user_id    = queueToUser msg
      postTarget = T.empty
      users      = (rmDup.Prelude.drop 3.T.words.gmt_text.V.head.mentions) msg
  postTweet msg postTarget botconf $ dm func
  return (V.empty, addUserInGroup user_id (T.pack "all") (pqGroups msg))

urmCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
urmCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "sudo") (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> do
  let user_id    = queueToUser msg
      users      = (rmDup.Prelude.drop 3.T.words.gmt_text.V.head.mentions) msg
      postTarget = T.empty
  postTweet msg postTarget botconf $ dm func
  return (V.empty, rmUserInGroup user_id (T.pack "all") (pqGroups msg))
  
uhelpCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
uhelpCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "all") (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> do
  let user_id    = (gur_id_str.gmt_user.V.head.mentions) msg
  postTarget <- TIO.readFile uHelpFile
  postTweet msg postTarget botconf $ dm func 
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
  postTarget <- TIO.readFile gHelpFile
  postTweet msg postTarget botconf $ dm func 
  return (V.empty, pqGroups msg)

-- error command part
errorCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text)) -- post error
errorCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "all") (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> do
  let postTarget = T.pack "command error..."
  postTweet msg postTarget botconf $ dm func
  return (V.empty, pqGroups msg)
