module CalcParser.Exec where

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

showTerm :: T.Text -> T.Text -> [String] -> IO(T.Text)
showTerm msg id conf = print msg >> print id >> return T.empty

-- post part
postTweet :: T.Text -> PostQueue -> T.Text -> [String] -> IO() 
postTweet usermsg postq postTarget botconf = if T.null postTarget then return () else do
 let msgs = splitN postTarget 140 -- 140 is twitter max size
 start_id <- showTerm (Prelude.head msgs) (T.empty) botconf -- Timeline
 postTweetInReply (Prelude.tail msgs) start_id
 showTerm usermsg ((gmid_str.V.head.mentions) postq) botconf
 return ()
 where
  splitN :: T.Text -> Int -> [T.Text] 
  splitN raw max = if T.length raw <= max then [raw] else (T.take max raw):splitN (T.drop max raw) max
  postTweetInReply :: [T.Text] -> T.Text -> IO()
  postTweetInReply [] _         = return ()
  postTweetInReply (x:xs) nowid = if T.null nowid then return () else do
   nextid <- showTerm x nowid botconf
   postTweetInReply xs nextid
-- tweet command part

twpostCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime))
twpostCmd msg botconf = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 1 >>= \x -> if not x then return V.empty else do
 let user_id  = (gid_str.gmuser.V.head.mentions) msg    -- twitter api name
     since_id = getTweetId msg -- twitter api name 
 userTL <- (\t -> case t of  Left e  -> error e
                             Right l -> (V.reverse.V.fromList) l) <$> getUserTL user_id since_id botconf
 let postmsg    = T.pack $ '@':(T.unpack.gid_str.gmuser.V.head.mentions) msg ++ "post done."
     postTarget = searchReplyTree since_id userTL -- search and sed
 postTweet postmsg msg postTarget botconf
 return V.empty

twbroadcastCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime))
twbroadcastCmd msg botconf = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 1 >>= \x -> if not x then return V.empty else do
 let user_id  = (gid_str.gmuser.V.head.mentions) msg    -- twitter api name
     since_id = getTweetId msg -- twitter api name
     postmsg  = T.pack $ '@':(T.unpack.gid_str.gmuser.V.head.mentions) msg ++ "post done."
 userTL <- (\t -> case t of  Left e  -> error e
                             Right l -> (V.reverse.V.fromList) l) <$> getUserTL user_id since_id botconf
 broadTarget <- broadUsers 
 let postTarget = T.append broadTarget (searchReplyTree since_id userTL)-- search and sed
 postTweet postmsg msg postTarget botconf
 return V.empty

twrmCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime))
twrmCmd msg botconf = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 1 >>= \x -> if not x then return V.empty else do
 let user_id  = (gid_str.gmuser.V.head.mentions) msg -- twitter api name
     since_id = getTweetId msg -- twitter api name
 userTL <- (\t -> case t of  Left e  -> error e
                             Right l -> (V.reverse.V.fromList) l) <$> getUserTL (T.pack "calc-mie") since_id botconf
 let postTarget = searchReplyId since_id userTL
     postmsg    = T.pack $ '@':(T.unpack.gid_str.gmuser.V.head.mentions) msg ++ "remove done."
 rmTweets postmsg msg postTarget botconf
 return V.empty

twHelpCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime))
twHelpCmd msg botconf = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 0 >>= \x -> if not x then return V.empty else do
 let postTarget = T.empty
 postmsg <- T.append (T.pack ('@':((T.unpack.gid_str.gmuser.V.head.mentions) msg))) <$> TIO.readFile twHelpFile
 postTweet postmsg msg postTarget botconf
 return V.empty

-- comming soon?
--twsetCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime))
--twsetCmd msg botconf = do

rmTweets :: T.Text -> PostQueue -> [T.Text] -> [String] -> IO()
rmTweets postmsg postq twid botconf = 
 if Prelude.null twid then tweet postmsg ((gmid_str.V.head.mentions) postq) botconf >> return () 
 else rmTweet (Prelude.head twid) botconf >> rmTweets postmsg postq (Prelude.tail twid) botconf

-- user command part
uaddCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime))
uaddCmd msg botconf = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 2 >>= \x -> if not x then return V.empty else do
 let user_id    = (gid_str.gmuser.V.head.mentions) msg
     postTarget = T.pack $ "done add user" ++ T.unpack (filterCmd msg 3)
     users      = (rmDup.Prelude.drop 3.T.words.gmtext.V.head.mentions) msg
 pusers <- Prelude.map splitChar.T.lines <$> TIO.readFile usersconf
 return V.empty

urmCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime))
urmCmd msg botconf = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 2 >>= \x -> if not x then return V.empty else do
 let user_id    = (gid_str.gmuser.V.head.mentions) msg
     postmsg    = T.pack $ "done rm user" ++ T.unpack (filterCmd msg 3)
     users      = (rmDup.Prelude.drop 3.T.words.gmtext.V.head.mentions) msg
     postTarget = T.empty
 pusers <- Prelude.map splitChar.T.lines <$> TIO.readFile usersconf
 TIO.writeFile usersconf ((T.unlines.Prelude.map tupleToString) (rmUserCmd users pusers))
 postTweet postmsg msg postTarget botconf
 return V.empty
  where
   rmUserCmd u p = if Prelude.null p then [] else 
                   if (((\(a,b,c,d) -> a).head) p) `elem` u then rmUserCmd u (Prelude.tail p) else
                   (head p):rmUserCmd u (Prelude.tail p)
   tupleToString (a,b,c,d) = T.pack $ (T.unpack a) ++ ',':boolToStr b ++ ',':boolToStr c ++ ',':boolToStr d
   boolToStr True  = "True"
   boolToStr False = "False"
 
usetCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime))
usetCmd msg botconf = do -- calc-tweet user set cmd user permit
 let cmd      = filterCmd msg 3 -- cmd = post, sudo, broadcast
     user     = filterCmd msg 4 -- user is target
     permit   = filterCmd msg 5 -- permit = True or False
     sudoflug = if T.unpack cmd == "sudo" || (T.unpack cmd /= "sudo" && T.unpack user /= "me") then 2 else 0 -- have to sudo or me
 getPermitUser ((gid_str.gmuser.V.head.mentions)msg) sudoflug >>= \x -> if not x then return V.empty else do
  users <- Prelude.map (permitRewrite user (cmd,strToBool permit).splitChar).T.lines <$> TIO.readFile usersconf
  return V.empty
  
uhelpCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime))
uhelpCmd msg botconf = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 0 >>= \x -> if not x then return V.empty else do
 let user_id    = (gid_str.gmuser.V.head.mentions) msg
     postTarget = T.empty
 postmsg <- TIO.readFile uHelpFile
 postTweet postmsg msg postTarget botconf
 return V.empty

-- group command part
---- calc-tweet group create name
--gcreateCmd :: PostQueue -> [String] -> (T.Text -> T.Text -> [String] -> IO(T.Text)) -> IO (V.Vector (T.Text, ZonedTime))
--gcreateCmd msg botconf func = getPermitUser((gid_str.gmuser.V.head.mentions)msg) 2 >>= \x -> if not x then return V.empty else do
--
---- calc-tweet group add group user
--gaddCmd :: PostQueue -> [String] -> (T.Text -> T.Text -> [String] -> IO(T.Text)) -> IO (V.Vector (T.Text, ZonedTime))
--gaddCmd msg botconf func = getPermitUser((gid_str.gmuser.V.head.mentions)msg) 2 >>= \x -> if not x then return V.empty else do
--
---- calc-tweet group rm group user
--grmCmd :: PostQueue -> [String] -> (T.Text -> T.Text -> [String] -> IO(T.Text)) -> IO (V.Vector (T.Text, ZonedTime))
--grmCmd msg botconf func = getPermitUser((gid_str.gmuser.V.head.mentions)msg) 2 >>= \x -> if not x then return V.empty else do
--
---- calc-tweet group delete name
--gdeleteCmd :: PostQueue -> [String] -> (T.Text -> T.Text -> [String] -> IO(T.Text)) -> IO (V.Vector (T.Text, ZonedTime))
--gdeleteCmd msg botconf func = getPermitUser((gid_str.gmuser.V.head.mentions)msg) 2 >>= \x -> if not x then return V.empty else do
--
---- calc-tweet group show name post DM 
--gshowCmd :: PostQueue -> [String] -> (T.Text -> T.Text -> [String] -> IO(T.Text)) -> IO (V.Vector (T.Text, ZonedTime))
--gshowCmd msg botconf func = getPermitUser((gid_str.gmuser.V.head.mentions)msg) 2 >>= \x -> if not x then return V.empty else do
--
---- calc-tweet group help
--ghelpCmd :: PostQueue -> [String] -> (T.Text -> T.Text -> [String] -> IO(T.Text)) -> IO (V.Vector (T.Text, ZonedTime))
--ghelpCmd msg botconf func = getPermitUser((gid_str.gmuser.V.head.mentions)msg) 0 >>= \x -> if not x then return V.empty else do

-- error command part
errorCmd :: PostQueue -> [String] -> IO (V.Vector (T.Text, ZonedTime)) -- post error
errorCmd msg botconf = getPermitUser ((gid_str.gmuser.V.head.mentions)msg) 0 >>= \x -> if not x then return V.empty else do
 let postmsg    = T.pack "command error..."
     postTarget = T.empty
 postTweet postmsg msg postTarget botconf
 return V.empty

