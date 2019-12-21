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
-- postTweet :: Lex -> T.Text -> BotsAPI -> (T.Text -> T.Text -> BotsAPI -> IO(T.Text)) -> IO() 
-- postTweet lex postTarget botconf func = if T.null postTarget then return () else do
--  func postTarget T.empty botconf
--  return ()
-- 
-- postDicectMessage :: Lex -> T.Text -> BotsAPI -> (T.Text -> T.Text -> BotsAPI -> IO(T.Text)) -> IO() 
-- postDicectMessage lex postTarget botconf func  = if T.null postTarget then return () else do
--  func postTarget (lex_user_id lex) botconf
--  return ()

-- tweet command part
twpostCmd :: Lex -> (Postfunc) -> Execfunc
twpostCmd lex func botconf = do
 userTL <- gtlToVector <$> getUserTL (lex_user_id lex) (first_id lex) (twitter botconf)
 let firstGetTL = getFirstGtl ((fst.first_id) lex) userTL 
     postTarget = (if (snd.first_id) lex then rtSearchReplyTree else repSearchReplyTree) firstGetTL userTL -- search and sed
 func postTarget T.empty botconf
 return (V.empty, V.empty)

twrmCmd :: Lex -> (Postfunc) -> Execfunc
twrmCmd lex func botconf =  do
  userTL <- gtlToVector <$> getUserTL (lex_user_id lex) (first_id lex) (twitter botconf)
  let postTarget = ((fst.first_id) lex):searchReplyId ((fst.first_id) lex) userTL
  rmTweets postTarget $ twitter botconf
  return (V.empty, V.empty)

twgroupCmd :: GandU -> Lex -> (Postfunc) -> Execfunc
twgroupCmd gandu lex func botconf = do
   userTL <- gtlToVector <$> getUserTL (lex_user_id lex) (first_id lex) (twitter botconf)
   let firstGetTL = getFirstGtl ((fst.first_id) lex) userTL
       searchfunc = if (snd.first_id) lex then rtSearchReplyTree else repSearchReplyTree
       postTarget = T.append (T.append ((replyUsers.snd) gandu) (T.singleton '\n')) (searchfunc firstGetTL userTL)
   func postTarget T.empty botconf
   return (V.empty, V.empty)
    where
     replyUsers :: V.Vector T.Text -> T.Text
     replyUsers users = T.append (T.singleton '@') ((T.intercalate (T.pack " @").V.toList) users)

--twshowCmd :: Lex -> Execfunc
--twshowCmd lex func botconf = do
--  userTL <- gtlToVector <$> getUserTL (lex_user_id lex) (first_id lex) (twitter botconf)
--  let firstGetTL = getFirstGtl ((fst.first_id) lex) userTL
--      postTarget = (if (snd.first_id) lex then rtSearchReplyTree else repSearchReplyTree) firstGetTL userTL -- search and sed
--  (dm func) postTarget (lex_user_id lex) botconf
----  postDicectMessage lex postTarget botconf $ dm func
--  return (V.empty, V.empty)

twHelpCmd :: Lex -> (Postfunc) -> Execfunc
twHelpCmd lex func botconf =  do
  postTarget <- TIO.readFile twHelpFile
  func postTarget (lex_user_id lex) botconf
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
gcreateCmd :: V.Vector GandU -> Lex -> (Postfunc) -> Execfunc
gcreateCmd gandu lex func botconf =  return (V.empty, addGroup (group lex) gandu)

-- calc-tweet group add group user
gaddCmd :: V.Vector GandU -> Lex -> (Postfunc) -> Execfunc
gaddCmd gandu lex func botconf = return (V.empty, addUsersInGroup (users lex) (group lex) gandu)

-- calc-tweet group rm group user
grmCmd :: V.Vector GandU -> Lex -> (Postfunc) -> Execfunc
grmCmd gandu lex func botconf = return (V.empty, rmUsersInGroup (users lex) (group lex) gandu)

-- calc-tweet group delete name
gdeleteCmd :: V.Vector GandU -> Lex -> (Postfunc) -> Execfunc
gdeleteCmd gandu lex func botconf = return (V.empty, deleteGroup (group lex) gandu)

gandushowCmd :: V.Vector GandU -> Lex -> (Postfunc) -> Execfunc
gandushowCmd gandu lex func botconf = do
 let postTarget = T.append (if (T.null.group) lex then T.empty else V.foldl1 (T.append) (usersInGroup (group lex) gandu))
                           (if (V.null.users) lex then T.empty else createPostTarget (users lex) gandu)
 func postTarget (lex_user_id lex) botconf
 return (V.empty, V.empty)
  where
   createPostTarget :: V.Vector T.Text -> V.Vector GandU -> T.Text
   createPostTarget us gandu = if V.null us then T.empty else
    case existUser (V.head us) gandu of
     False -> T.pack "unjoined" 
     True  -> T.append (V.head us) $ T.append (T.singleton '\n') $ T.append (groupInUser (V.head us) gandu) 
                                   $ T.append (T.pack "\n--------------------\n") $ createPostTarget (V.tail us) gandu
 

-- calc-tweet group help
ghelpCmd :: V.Vector GandU -> Lex -> (Postfunc) -> Execfunc
ghelpCmd gandu lex func botconf = do
 postTarget <- TIO.readFile gHelpFile
 func postTarget (lex_user_id lex) botconf
 return (V.empty, V.empty)

-- help command part
allhelpCmd :: Lex -> (Postfunc) -> Execfunc
allhelpCmd lex func botconf = do
 helps <- TIO.readFile helpFile
 twhelp <- TIO.readFile twHelpFile
 ghelp <- TIO.readFile gHelpFile
 let postTarget = T.append helps $ T.append twhelp ghelp
 func postTarget (lex_user_id lex) botconf
 return (V.empty, V.empty) 

-- error command part
errorCmd :: T.Text -> Lex -> (Postfunc) -> Execfunc -- post error
errorCmd text lex func botconf = do
 let postTarget = T.append (T.pack "error : ") text
 func postTarget (lex_user_id lex) botconf
 return (V.empty, V.empty)
