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
monitoring :: MVar PostQueue -> T.Text -> BotsAPI -> Postfunc -> IO ()
monitoring msgq since_id botconf func = do
 threadDelay mentiont
 --(rtCheck pd >>= remindCheck typeTL)-- monitoring retweeting
 tlmention <- (\t -> case t of Left  e -> error e
                               Right l -> (V.reverse.V.fromList) l) <$> getMention since_id (twitter botconf)
 if V.null tlmention then monitoring msgq since_id botconf func
 else do
  befq <- takeMVar msgq 
  (if (V.null.mentions) befq then do
   putMVar msgq befq {mentions = V.filter filterCmdCalcTweet tlmention}
   forkIO $ cmdCheck msgq botconf func
   return ()
  else do
--   befq <- takeMVar msgq 
   putMVar msgq befq{mentions = ((mentions befq) V.++ (V.filter filterCmdCalcTweet tlmention))}
   ) >> monitoring msgq ((gmt_id_str.V.head) tlmention) botconf func
 where
  filterCmdCalcTweet :: GetMention -> Bool
  filterCmdCalcTweet tw = isEqStrText "calc-tweet" ((Prelude.head.T.words.gmt_text) tw)

cmdCheck :: MVar PostQueue -> BotsAPI -> Postfunc -> IO ()
cmdCheck msgq botconf postfunc = readMVar msgq >>= \nowq -> if (V.null.mentions) nowq then return () else do
 let command = case T.unpack (filterCmd nowq 1) of
                    "tweet" -> tweetCmd nowq 
                    "user"  -> userCmd nowq
                    "group" -> groupCmd nowq
--                  "web"   -> webCmd nowq
                    "help"  -> allhelpCmd
                    _       -> errorCmd
 (sc, pqgroup) <- command nowq botconf postfunc
 addDeleteSchedule msgq sc pqgroup  -- add or delete schedule 
 threadDelay cmdt
 TIO.writeFile groupsconf $ T.unlines.V.toList.V.map (commaIns.V.toList.(\(x, y) -> V.cons x y)) $ pqgroup
 cmdCheck msgq botconf postfunc
  where
   addDeleteSchedule q d g = takeMVar q >>= \x -> putMVar q x { mentions = if (V.null.mentions) x then V.empty else  (V.tail.mentions)x
                                                              , schedule = if V.null d then schedule x else schedule x V.++ d
                                                              , pqGroups = g} 
   cmdt = 60*1000*1000 -- 1min

-- tweet command 
tweetCmd :: PostQueue -> (PostQueue -> BotsAPI -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text)))
tweetCmd msg = case T.unpack (filterCmd msg 2) of
 "post"      -> twpostCmd
 "rm"        -> twrmCmd 
 "help"      -> twHelpCmd
 "show"      -> twshowCmd
 _           -> twgroupCmd

-- user command
userCmd :: PostQueue -> (PostQueue -> BotsAPI -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text)))
userCmd msg = case T.unpack (filterCmd msg 2) of
 "add"  -> uaddCmd
 "rm"   -> urmCmd 
 "help" -> uhelpCmd
 "show" -> ushowCmd
 _      -> errorCmd

-- web command comming soon?
--webCmd :: MVar PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime)) -- post web
--webCmd msg botconf postdata = calcWebPost postdata msg botconf typeTL

groupCmd :: PostQueue -> (PostQueue -> BotsAPI -> Postfunc -> IO (V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text)))
groupCmd msg = case T.unpack (filterCmd msg 2) of
 "create" -> gcreateCmd
 "add"    -> gaddCmd
 "rm"     -> grmCmd 
 "delete" -> gdeleteCmd
 "show"   -> gshowCmd
 "help"   -> ghelpCmd
 _        -> errorCmd

