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
  befq <- takeMVar msgq  -- get and stop other threads
  let queues = V.filter (\x -> existUser (gmtToSN x) (pqGroups befq)) tlmention
  (if (V.null.mentions) befq then do
    putMVar msgq befq {mentions = queues} -- start thread 
    forkIO $ cmdCheck func msgq botconf -- create thread
    return ()
   else putMVar msgq befq{mentions = ((mentions befq) V.++ queues)}) -- start thread
  >> monitoring func msgq ((gmt_id_str.V.head) tlmention) botconf
 where
  filterCmdCalcTweet :: GetMention -> Bool
  filterCmdCalcTweet tw = isEqStrText "calc-tweet" ((Prelude.head.T.words.gmt_text) tw) -- only first string "calc-tweet"
  gmtToVector :: Either String [GetMention] -> V.Vector GetMention
  gmtToVector list = case list of Left  e -> V.empty
                                  Right r -> (V.reverse.V.fromList) r

cmdCheck :: Postfunc -> MVar PostQueue -> BotsAPI -> IO ()
cmdCheck postfunc msgq botconf = readMVar msgq >>= \nowq -> if (V.null.mentions) nowq then return () else do 
 let lex     = lexAnalyser ((V.head.mentions) nowq)
 let command = parseCmd lex (pqGroups nowq) ((gmtToUI.V.head.mentions) nowq)
 (sc, group) <- command postfunc botconf
 TIO.writeFile groupsconf $ T.unlines.V.toList.V.map (commaIns.V.toList.(\(x, y) -> V.cons x y)) $ if V.null group then pqGroups nowq else group
 addDeleteSchedule msgq sc group -- add or delete schedule 
 threadDelay cmdt
 cmdCheck postfunc msgq botconf 
  where
   addDeleteSchedule q d g = takeMVar q >>= \x -> putMVar q x { mentions = if (V.null.mentions) x then V.empty else  (V.tail.mentions)x
                                                              , schedule = if V.null d then schedule x else schedule x V.++ d
                                                              , pqGroups = if V.null g then pqGroups x else g}
   cmdt = 60*1000*1000 -- 1min

parseCmd :: Either T.Text Lex -> V.Vector GandU -> T.Text -> (Func)
parseCmd lex gandu id = case lex of 
 Left s  -> errorCmd s lexAllNull { lex_user_id = id }
 Right l -> case (T.unpack.subcmd) l of
  "post"   -> postSelector l gandu
  "show"   -> showSelector l gandu
  "add"    -> if (or.map (\x -> existInGroup (lex_screen_name l) x gandu)) [T.pack "sudo",group l] 
               then gaddCmd gandu l
               else errorCmd (T.pack "permission denied") l
  "rm"     -> rmSelector l gandu
  "create" -> if existInGroup (lex_screen_name l) (T.pack "sudo") gandu then gcreateCmd gandu l
                                                                        else errorCmd (T.pack "permission denied") l
  "delete" -> if existInGroup (lex_screen_name l) (T.pack "sudo") gandu then gdeleteCmd gandu l
                                                                        else errorCmd (T.pack "permission denied") l
  "help"   -> allhelpCmd l
  _        -> errorCmd (T.append (subcmd l) (T.pack " not found.")) l

showSelector :: Lex -> V.Vector GandU -> (Func)
showSelector lex gandu = case ((T.null.fst.first_id) lex, (T.null.group) lex || (V.null.users) lex) of
 (True, True)  -> errorCmd (T.pack "target which you want to check isn`t selected") lex
 (True, False) -> gandushowCmd gandu lex
 (False, True) -> twshowCmd lex
 otherwise     -> errorCmd (T.pack ".......") lex

rmSelector :: Lex -> V.Vector GandU -> (Func)
rmSelector lex gandu = case  ((T.null.fst.first_id) lex, (T.null.group) lex, (V.null.users) lex) of
 (False , True, True)  -> errorCmd (T.pack "target which you want to remove isn't selected") lex
 (False , True, False) -> if (or.map (\x -> existInGroup (lex_screen_name lex) x gandu)) [T.pack "sudo", group lex]
                           then grmCmd gandu lex
                           else errorCmd (T.pack "permission denied") lex
 (True, False, False)  -> if existInGroup (lex_screen_name lex) (T.pack "post") gandu then twrmCmd  lex
                                                                                      else errorCmd (T.pack "permission denied") lex
 otherwise             -> errorCmd (T.pack "which is target?") lex

postSelector :: Lex -> V.Vector GandU -> (Func)
postSelector lex gandu = case ((T.null.group) lex, existInGroup (lex_screen_name lex) (T.pack "post") gandu) of
 (True, True)  -> case (V.find ((==(group lex)).fst) gandu) of Nothing -> errorCmd (T.append (group lex) (T.pack " not found.")) lex
                                                               Just a  -> twgroupCmd a lex
 (False, True) -> twpostCmd lex
 _             -> errorCmd (T.pack "permission denied") lex

