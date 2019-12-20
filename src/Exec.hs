module Exec where

import SlackAPI
import TwitterAPI
import DiscordAPI
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
postTweet :: Lex -> T.Text -> BotsAPI -> (T.Text -> T.Text -> [String] -> IO(T.Text)) -> IO() 
postTweet lex postTarget botconf func = if T.null postTarget then return () else do
 postSlack postTarget $ slack botconf
 sendMessageDiscord postTarget $ discord botconf
 let msgs = splitN postTarget 140 -- 140 is twitter max size
 start_id <- func (Prelude.head msgs) (T.empty) $ twitter botconf -- Timeline
 if (not.Prelude.null.Prelude.tail) msgs then postTweetInReply (Prelude.tail msgs) start_id func else return ()
 where
  splitN :: T.Text -> Int -> [T.Text] 
  splitN raw max = if T.length raw <= max then [raw] else (T.take max raw):splitN (T.drop max raw) max
  postTweetInReply :: [T.Text] -> T.Text -> (T.Text -> T.Text -> [String] -> IO(T.Text)) -> IO()
  postTweetInReply [] _ _            = return ()
  postTweetInReply (x:xs) nowid func = if T.null nowid then return () else do
   nextid <- func x nowid $ twitter botconf
   postTweetInReply xs nextid func

postDicectMessage :: Lex -> T.Text -> BotsAPI -> (T.Text -> T.Text -> [String] -> IO(T.Text)) -> IO() 
postDicectMessage lex postTarget botconf func  = if T.null postTarget then return () else do
 func postTarget (lex_user_id lex) $ twitter botconf
 return ()
 where
  postTweetInReply :: [T.Text] -> T.Text -> (T.Text -> T.Text -> [String] -> IO(T.Text)) -> IO()
  postTweetInReply x uid func = if Prelude.null x then return () else do
   func (Prelude.head x) uid $ twitter botconf
   postTweetInReply (Prelude.tail x) uid func

-- tweet command part
twpostCmd :: Func
twpostCmd func lex botconf = do
 userTL <- gtlToVector <$> getUserTL (lex_user_id lex) ((beforeId.first_id) lex) (twitter botconf) -- reply だと先頭取れない? (apiみろ)
 let firstGetTL = getFirstGtl (first_id lex) userTL 
     postTarget = searchReplyTree firstGetTL userTL -- search and sed
 postTweet lex postTarget botconf $ tl func
 return (V.empty, V.empty)

twrmCmd :: Func
twrmCmd func lex botconf =  do
  userTL <- gtlToVector <$> getUserTL (lex_user_id lex) ((beforeId.first_id) lex) (twitter botconf)
  let postTarget = (first_id lex):searchReplyId (first_id lex) userTL
  rmTweets postTarget $ twitter botconf
  return (V.empty, V.empty)

twgroupCmd :: GandU -> Func
twgroupCmd gandu func lex botconf = do
   userTL <- gtlToVector <$> getUserTL (lex_user_id lex) ((beforeId.first_id) lex) (twitter botconf)
   let firstGetTL = getFirstGtl (first_id lex) userTL
       postTarget = T.append (T.append ((replyUsers.snd) gandu) (T.singleton '\n')) (searchReplyTree firstGetTL userTL)
   postTweet lex postTarget botconf $ tl func
   return (V.empty, V.empty)
    where
     replyUsers :: V.Vector T.Text -> T.Text
     replyUsers users = T.append (T.singleton '@') ((T.intercalate (T.pack " @").V.toList) users)

twshowCmd :: Func
twshowCmd func lex botconf = do
  userTL <- gtlToVector <$> getUserTL (lex_user_id lex) ((beforeId.first_id) lex) (twitter botconf)
  let firstGetTL = getFirstGtl (first_id lex) userTL
      postTarget = searchReplyTree firstGetTL userTL -- search and sed
  postDicectMessage lex postTarget botconf $ dm func
  return (V.empty, V.empty)

twHelpCmd :: Func
twHelpCmd func lex botconf =  do
  postTarget <- TIO.readFile twHelpFile
  postDicectMessage lex postTarget botconf $ dm func
  return (V.empty, V.empty)

-- comming soon?
--twsetCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime))
--twsetCmd msg botconf = do

rmTweets :: [T.Text] -> [String] -> IO()
rmTweets twid botconf = 
 if Prelude.null twid then return () 
 else rmTweet (Prelude.head twid) botconf >> rmTweets (Prelude.tail twid) botconf

-- group command part
-- calc-tweet group create name
gcreateCmd :: V.Vector GandU -> Func
gcreateCmd gandu func lex botconf =  return (V.empty, addGroup (group lex) gandu)

-- calc-tweet group add group user
gaddCmd :: V.Vector GandU -> Func
gaddCmd gandu func lex botconf = return (V.empty, addUsersInGroup (users lex) (group lex) gandu)

-- calc-tweet group rm group user
grmCmd :: V.Vector GandU -> Func
grmCmd gandu func lex botconf = return (V.empty, rmUsersInGroup (users lex) (group lex) gandu)

-- calc-tweet group delete name
gdeleteCmd :: V.Vector GandU -> Func
gdeleteCmd gandu func lex botconf = return (V.empty, deleteGroup (group lex) gandu)

gandushowCmd :: V.Vector GandU -> Func
gandushowCmd gandu func lex botconf = do
 let postTarget = (if (T.null.group) lex then [] else groupInUser (group lex) gandu)  
                  ++ (if (V.null users) lex then [] else createPostTarget (users lex) gandu)
 postDicectMessage lex postTarget botconf $ dm func
 return (V.empty, V.empty)
  where
   createPostTarget :: V.Vector T.Text -> V.Vector GandU -> T.Text
   createPostTarget us gandu = if V.null us then T.empty else
    case existUser (V.head us) gandu of
     False -> T.pack "unjoined" 
     True  -> T.append $ T.append (V.head us) $ T.append (T.singleton '\n') $ T.append $ groupInUser (V.head us) gandu 
                       $ T.append (T.pack "--------------------\n") $ createPostTarget (V.tail us) gandu
 

-- calc-tweet group help
ghelpCmd :: V.Vector GandU -> Func
ghelpCmd gandu func lex botconf = do
  postTarget <- TIO.readFile gHelpFile
  postDicectMessage lex postTarget botconf $ dm func 
  return (V.empty, V.empty)

-- help command part
allhelpCmd :: Func
allhelpCmd func lex botconf = do
  helps <- TIO.readFile helpFile
  twhelp <- TIO.readFile twHelpFile
--  uhelp <- TIO.readFile uHelpFile
  ghelp <- TIO.readFile gHelpFile
  let postTarget = T.append helps $ T.append twhelp ghelp
  postDicectMessage lex postTarget botconf $ dm func 
  return (V.empty, V.empty) 

-- error command part
errorCmd :: T.Text -> Func -- post error
errorCmd text func lex botconf = do
  let postTarget = T.append (T.pack "error : ") text
  postDicectMessage lex postTarget botconf $ dm func
  return (V.empty, V.empty)
