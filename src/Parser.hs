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
monitoring :: Postfunc -> MVar PostQueue -> T.Text -> BotsAPI -> IO ()
monitoring func msgq since_id botconf = do
 threadDelay mentiont
 tlmention <- gmtToVector <$> getMention since_id (twitter botconf)
 if V.null tlmention then monitoring func msgq since_id botconf else do
  let queues = V.filter (\x -> existUser (gmtToSN x) (bApiGroup botconf)) tlmention
  befq <- takeMVar msgq  -- get and stop other threads
  (if (V.null.mentions) befq then do
    putMVar msgq befq {mentions = queues} -- start thread 
    forkIO $ cmdCheck func msgq botconf -- create thread
    return ()
   else putMVar msgq befq{mentions = ((mentions befq) V.++ queues}) -- start thread
  >> monitoring func msgq ((gmt_id_str.V.head) tlmention) botconf
 where
  filterCmdCalcTweet :: GetMention -> Bool
  filterCmdCalcTweet tw = isEqStrText "calc-tweet" ((Prelude.head.T.words.gmt_text) tw) -- only first string "calc-tweet"
  gmtToVector :: Either String [GetMention] -> V.Vector GetMention
  gmtToVector list = case list of Left  e -> V.empty
                                  Right r -> (V.reverse.V.fromList) r

cmdCheck :: Postfunc -> MVar PostQueue -> BotsAPI -> IO ()
cmdCheck postfunc msgq botconf = readMVar msgq >>= \nowq -> if (V.null.mentions) nowq then return () else do 
 let command = ParseCmd ((V.head.mentions) nowq) (pqGroup nowq)
 (sc, group) <- command postfunc botconf
 addDeleteSchedule msgq sc -- add or delete schedule 
 threadDelay cmdt
 TIO.writeFile groupsconf $ T.unlines.V.toList.V.map (commaIns.V.toList.(\(x, y) -> V.cons x y)) $ group
 cmdCheck postfunc msgq botconf 
  where
   addDeleteSchedule q d g = takeMVar q >>= \x -> putMVar q x { mentions = if (V.null.mentions) x then V.empty else  (V.tail.mentions)x
                                                              , schedule = if V.null d then schedule x else schedule x V.++ d
                                                              . pqGroup  = if V.null g then pqGroup x else g}
   cmdt = 60*1000*1000 -- 1min

parseCmd :: GetMention -> V.Vector GandU -> (Func)
parseCmd gmt gandu = let lex = lexAnalyser gmt in case lex of 
 Right s -> errorCmd s
 Left l  -> case (T.unpack.subcmd) l of
  "post"   -> postSelector gmt l gandu
  "show"   -> showSelector gmt l gandu
  "add"    -> if (or.map (\x -> existInGroup (gmtToSN gmt) x)) [T.pack "sudo",filterCmd msg 3] 
               then gaddCmd (group l) (users l) 
               else errorCmd (T.pack "permission denied") (gmtToUI gmt)
  "rm"     -> rmSelector gmt l gandu
  "create" -> if existInGroup (gmtToSN gmt) (T.pack "sudo") then gcreateCmd (group lex)
                                                            else errorCmd (T.pack "permission denied") (gmtToUI gmt)
  "delete" -> if existInGroup (gmtToSN gmt) (T.pack "sudo") then gdeleteCmd (group lex)
                                                            else errorCmd (T.pack "permission denied") (gmtToUI gmt)
  "help"   -> allhelpCmd (getUserIdFromGmt gmt) 
  _        -> errorCmd (T.append (subcmd l) (T.pack " not found."))

showSelector :: GetMention -> Lex -> V.Vector GandU -> V.Vector GandU -> (Func)
showSelector gmt lex gandu = case (gmtToRpStatus gmt, gmt_entities gmt, group lex, users lex) of
 (Nothing, [] , T.empty, V.empty) -> errorCmd (T.pack "target which you want to check isn`t selected")
 (Nothing, [] , T.empty, users)   -> ushowCmd users
 (Nothing, [] , group  , V.empty) -> gshowCmd (gmtToUI gmt) (groupInUser group gandu)
 (Nothing, [x], group  , V.empty) -> twshowCmd x
 (Just a , [] , group  , V.empty) -> twshowCmd a
 otherwise                        -> errorCmd (T.pack ".......")

rmSelector :: GetMention -> Lex -> V.Vector GandU -> (Func)
rmSelector gmt lex gandu = case  (gmtToRpStatus gmt, gmt_entities gmt, group lex, users lex) of
 (Nothing, [] , T.empty, V.empty) -> errorCmd (T.pack "target which you want to remove isn't selected") (gmtToUI gmt)
 (Nothing, [] , group  , users)   -> if (or.map (\x -> existInGroup (gmtToSN gmt) x gandu)) [T.pack "sudo", group lex] 
                                                                                   then grmCmd group (gmtToUI gmt)
                                                                                   else errorCmd (T.pack "permission denied") (gmtToUI gmt)
 (Nothing, [x], T.empty, T.empty) -> if existInGroup (gmtToSN gmt) (T.pack "post") then twrmCmd (gmtToUI gmt) x
                                                                                   else errorCmd (T.pack "permission denied")
 (Just a , [] , T.empty, T.empty) -> if existInGroup (gmtToSN gmt) (T.pack "post") then twrmCmd (gmtToUI gmt) a
                                                                                   else errorCmd (T.pack "permission denied")
 otherwise                        -> errorCmd (T.pack ".......") (gmtToUI gmt)

postSelector :: GetMention -> Lex -> (Postfunc -> BotsAPI -> IO(V.Vector (T.Text, ZonedTime), V.Vector (T.Text, V.Vector T.Text)))
postSelector gmt lex = case (group lex) of
 T.empty -> if existInGroup (gmtToSN gmt) (T.pack "post") then twgroupCmd (gmtToUI gmt) (getTweetNextId gmt) users
                                                          else errorCmd (T.pack "permission denied") (gmtToUI gmt)
 _       -> if existInGroup (gmtToSN gmt) (T.pack "post") then twpostCmd (gmtToUI gmt) (getTweetNextId gmt)
                                                          else errorCmd (T.pack "permission denied") (gmtToUI gmt)

