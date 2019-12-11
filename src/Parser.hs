module Parser where

import SlackAPI
import TwitterAPI
import Lib
import Exec

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

-- main call this function
monitoring :: MVar PostQueue -> T.Text -> [String] -> Postfunc -> IO ()
monitoring msgq since_id botconf func = do
 threadDelay mentiont
 --(rtCheck pd >>= remindCheck typeTL)-- monitoring retweeting
 tlmention <- (\t -> case t of Left  e -> error e
                               Right l -> (V.reverse.V.fromList) l) <$> getMention since_id botconf
 puser <- V.fromList.Prelude.map splitChar.T.lines <$> TIO.readFile usersconf
 if V.null tlmention then monitoring msgq since_id botconf
 else do
  nowq <- readMVar msgq 
  (if (V.null.mentions) nowq then do
   putMVar msgq nowq {mentions = V.filter (\x -> filterUserElem x puser && filterCmdCalcTweet x) tlmention}
   forkIO $ cmdCheck msgq botconf func
   return ()
  else do
   befq <- takeMVar msgq 
   putMVar msgq befq{mentions = ((mentions befq) V.++ (V.filter (\x -> filterUserElem x puser && filterCmdCalcTweet x) tlmention))}
   ) >> monitoring msgq ((gmid_str.V.head) tlmention) botconf
 where
  filterUserElem :: GetMention -> V.Vector (T.Text, Bool, Bool, Bool) -> Bool
  filterUserElem x   = V.elem ((gid_str.gmuser) x).V.map (\(a,b,c,d) -> a)
  filterCmdCalcTweet = (=="calc-tweet").T.unpack.Prelude.head.T.words.gmtext

cmdCheck :: MVar PostQueue -> [String] -> Postfunc -> IO ()
cmdCheck msgq botconf postfunc = readMVar msgq >>= \nowq -> if (V.null.mentions) nowq then return () else do
 let command = (case T.unpack (filterCmd nowq 1) of
                     "tweet" -> tweetCmd 
                     "user"  -> userCmd
--                     "group" -> groupCmd // comming soon
--                   "web"   -> webCmd
                     _       -> errorCmd) nowq
 sc <- command nowq botconf postfunc
 addDeleteSchedule msgq sc  -- add or deleteschedule 
 threadDelay cmdt
 cmdCheck msgq botconf
  where
   addDeleteSchedule q d = takeMVar msgq >>= \x -> putMVar msgq x{ mentions = (V.tail.mentions)x
                                                                 , schedule =  if V.null d then schedule x else schedule x V.++ d} 
   cmdt = 60*1000*1000 -- 1min

-- tweet command 
tweetCmd :: PostQueue -> (PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime)))
tweetCmd msg = case T.unpack (filterCmd msg 2) of
 "post"      -> twpostCmd
 "broadcast" -> twbroadcastCmd 
 "rm"        -> twrmCmd 
 "help"      -> twHelpCmd
 "show"      -> twShowCmd
-- "set"       -> twsetCmd 
 _           -> errorCmd

-- user command
userCmd :: PostQueue -> (PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime)))
userCmd msg = case T.unpack (filterCmd msg 2) of
 "add"  -> uaddCmd
 "rm"   -> urmCmd 
-- "set"  -> usetCmd
 "help" -> uhelpCmd
 "show" -> ushowCmd
 _      -> errorCmd

-- web command comming soon?
--webCmd :: MVar PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime)) -- post web
--webCmd msg botconf postdata = calcWebPost postdata msg botconf typeTL

groupCmd :: PostQueue -> (PostQueue -> [String] -> Postfunc -> IO (V.Vector (T.Text, ZonedTime)))
groupCmd msg = case T.unpack (filterCmd msg 2) of
 "create" -> gcreateCmd
 "add"    -> gaddCmd
 "rm"     -> grmCmd 
 "delete" -> gdeleteCmd
 "show"   -> gshowCmd
 "help"   -> ghelpCmd
 _        -> errorCmd

