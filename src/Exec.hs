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

-- post tweet
postTweet :: PostQueue -> T.Text -> [String] -> (T.Text -> T.Text -> [String] -> IO(T.Text)) -> IO() 
postTweet postq postTarget botconf func = if T.null postTarget then return () else do
 let msgs = splitN postTarget 140 -- 140 is twitter max size
 start_id <- func (Prelude.head msgs) (T.empty) botconf -- Timeline
 if (not.Prelude.null.Prelude.tail) msgs then postTweetInReply (Prelude.tail msgs) start_id func else return ()
 where
  splitN :: T.Text -> Int -> [T.Text] 
  splitN raw max = if T.length raw <= max then [raw] else (T.take max raw):splitN (T.drop max raw) max
  postTweetInReply :: [T.Text] -> T.Text -> (T.Text -> T.Text -> [String] -> IO(T.Text)) -> IO()
  postTweetInReply [] _ _            = return ()
  postTweetInReply (x:xs) nowid func = if T.null nowid then return () else do
   nextid <- func x nowid botconf
   postTweetInReply xs nextid func

postDicectMessage :: PostQueue -> T.Text -> [String] -> (T.Text -> T.Text -> [String] -> IO(T.Text)) -> IO() 
postDicectMessage postq postTarget botconf func  = if T.null postTarget then return () else do
 let msgs    = splitN postTarget 140 -- 140 is twitter max size
     user_id = (gur_id_str.gmt_user.V.head.mentions) postq
 if (not.Prelude.null) msgs then postTweetInReply msgs user_id func else return ()
 where
  splitN :: T.Text -> Int -> [T.Text] 
  splitN raw max = if T.length raw <= max then [raw] else (T.take max raw):splitN (T.drop max raw) max
  postTweetInReply :: [T.Text] -> T.Text -> (T.Text -> T.Text -> [String] -> IO(T.Text)) -> IO()
  postTweetInReply x uid func = if Prelude.null x then return () else do
   func (Prelude.head x) uid botconf
   postTweetInReply (Prelude.tail x) uid func

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
                              Right l -> (V.reverse.V.fromList) l) <$> getUserTL (T.pack botscreen_name) since_id botconf
  let postTarget = since_id:searchReplyId since_id userTL
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
                    else T.append (T.append (replyUsers users) (T.singleton '\n')) (searchReplyTree firstGetTL userTL) -- search and sed
   postTweet msg postTarget botconf $ tl func
   return (V.empty, pqGroups msg)
    where
     replyUsers :: V.Vector T.Text -> T.Text
     replyUsers users = T.append (T.singleton '@') ((T.intercalate (T.pack " @").V.toList) users)

twshowCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
twshowCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "post") (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> do
  let user_id  = (gur_id_str.gmt_user.V.head.mentions) msg    -- twitter api name
      since_id = getTweetNextId msg -- twitter api name 
  userTL <- (\t -> case t of  Left e  -> error e
                              Right l -> (V.reverse.V.fromList) l) <$> getUserTL user_id since_id botconf
  let firstGetTL = ((\t -> case t of Nothing -> getTLAllNULL
                                     Just t  -> t).V.find ((==(getTweetId msg)).gtl_id_str)) userTL
      postTarget = searchReplyTree firstGetTL userTL -- search and sed
  postDicectMessage msg postTarget botconf $ dm func
  return (V.empty, pqGroups msg)

twHelpCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
twHelpCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "all") (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> do
  postTarget <- TIO.readFile twHelpFile
  postDicectMessage msg postTarget botconf $ dm func
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
      adduser    = filterCmd msg 3
--  postDicectMessage msg postTarget botconf $ dm func
  return (V.empty, addUserInGroup adduser (T.pack "all") (pqGroups msg))

urmCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
urmCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "sudo") (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> do
  let user_id    = queueToUser msg
      rmuser     = filterCmd msg 3
      postTarget = T.empty
--  postDicectMessage msg postTarget botconf $ dm func
  return (V.empty, rmUserInGroup rmuser (T.pack "all") (pqGroups msg))
  
uhelpCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
uhelpCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "all") (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> do
  let user_id    = (gur_id_str.gmt_user.V.head.mentions) msg
  postTarget <- TIO.readFile uHelpFile
  postDicectMessage msg postTarget botconf $ dm func 
  return (V.empty, pqGroups msg)

ushowCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
ushowCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "all") (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> do
  let checkUser  = filterCmd msg 3
  let user_id    = (gur_id_str.gmt_user.V.head.mentions) msg
      postTarget = case existUser checkUser (pqGroups msg) of
                         False -> T.pack "unjoined" 
                         True  -> T.append checkUser $ T.append (T.singleton '\n') $ userInGroups checkUser (pqGroups msg) 
  postDicectMessage msg postTarget botconf $ dm func 
  return (V.empty, pqGroups msg)

-- group command part
-- calc-tweet group create name
gcreateCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
gcreateCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "sudo") (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> return (V.empty, addGroup (filterCmd msg 3) (pqGroups msg))

-- calc-tweet group add group user
gaddCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
gaddCmd msg botconf func = case (or.map (\x -> existInGroup (queueToUser msg) x (pqGroups msg))) [T.pack "sudo",filterCmd msg 3] of
 False -> return (V.empty, pqGroups msg)
 True  -> do
  let group   = filterCmd msg 3
  let addUser = filterCmd msg 4
  return (V.empty, addUserInGroup addUser group (pqGroups msg))

-- calc-tweet group rm group user
grmCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
grmCmd msg botconf func = case (or.map (\x -> existInGroup (queueToUser msg) x (pqGroups msg))) [T.pack "sudo",filterCmd msg 3] of 
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
gshowCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
gshowCmd msg botconf func = case existInGroup (queueToUser msg) (filterCmd msg 3) (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> do
  let user_id    = (gur_id_str.gmt_user.V.head.mentions) msg
      group      = filterCmd msg 3
      postTarget = userInGroups group (pqGroups msg)
  postDicectMessage msg postTarget botconf $ dm func 
  return (V.empty, pqGroups msg)

-- calc-tweet group help
ghelpCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text))
ghelpCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "all") (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> do
  let user_id    = (gur_id_str.gmt_user.V.head.mentions) msg
  postTarget <- TIO.readFile gHelpFile
  postDicectMessage msg postTarget botconf $ dm func 
  return (V.empty, pqGroups msg)

-- error command part
errorCmd :: PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text)) -- post error
errorCmd msg botconf func = case existInGroup (queueToUser msg) (T.pack "all") (pqGroups msg) of
 False -> return (V.empty, pqGroups msg)
 True  -> do
  let postTarget = T.pack "command error..."
  postDicectMessage msg postTarget botconf $ dm func
  return (V.empty, pqGroups msg)
