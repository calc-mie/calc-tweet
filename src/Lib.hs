module Lib where

import TwitterAPI
import SlackAPI

import Control.Exception
import Control.Concurrent
import System.Directory
import System.IO
import Control.Exception
import Data.Time
import qualified Data.List as L
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Text.Lazy as TLazy
import qualified Data.Text.Lazy.IO as DTLIO
import qualified Data.Vector as V

-- have to MVector
data PostQueue = PostQueue { mentions :: V.Vector GetMention
                           , schedule :: V.Vector (T.Text, ZonedTime)-- twid, retweet time
                           , pqGroups :: V.Vector (T.Text, V.Vector T.Text)
                           } deriving (Show)

data Week = Monday
          | Tuesday
          | Wednesday
          | Thursday
          | Friday
          | Saturday
          | Sunday
          deriving (Show, Enum, Eq)

data BotsAPI = BotsAPI { twitter :: [String]
                       , slack   :: [String]
                       , discord :: String}

data Postfunc = Postfunc { tl       :: T.Text -> T.Text -> [String] -> IO(T.Text)
                         , dm       :: T.Text -> T.Text -> [String] -> IO(T.Text)}

getUserAllNULL = User { gur_id_str = T.empty
                      , gur_screen_name = T.empty}

getTLAllNULL = GetTL { gtl_text = T.empty
                     , gtl_id_str = T.empty
                     , gtl_in_reply_to_status_id_str = Nothing
                     , gtl_user = getUserAllNULL }

botscreen_name = "flow_6852"
helpFile = "/usr/local/calc-tweet/helps/help.txt" 
twHelpFile = "/usr/local/calc-tweet/helps/tweet_help.txt"
uHelpFile = "/usr/local/calc-tweet/helps/users_help.txt"
gHelpFile = "/usr/local/calc-tweet/helps/groups_help.txt"
groupsconf = "/usr/local/calc-tweet/groups.conf"

emptyint = 1*1000*1000  :: Int {- 1 second -} 
mentiont = 12*1000*1000 :: Int {-12 second -}

searchReplyTree :: GetTL -> V.Vector GetTL -> T.Text -- create all message
searchReplyTree tl tls = if T.null (gtl_id_str tl) then T.empty else do
 let replys = V.filter ((==gtl_id_str tl).rpStatus) tls
     (seds, next) = sedsNext replys
 T.append (restoreSeds (gtl_text tl) (V.map gtl_text seds)) $ T.append (T.singleton '\n') $ searchReplyTree next tls
  where
   sedsNext :: V.Vector GetTL -> (V.Vector GetTL, GetTL)
   sedsNext rep = case V.find (not.isEqStrText "calc-tweet".(`getTlToCmd` 0).gtl_text) rep of
    Nothing -> (V.filter (isEqStrText "calc-tweet".(`getTlToCmd` 0).gtl_text) rep, getTLAllNULL)
    Just a  -> (V.filter (isEqStrText "calc-tweet".(`getTlToCmd` 0).gtl_text) rep, a)
   restoreSeds :: T.Text -> V.Vector T.Text -> T.Text
   restoreSeds text sed = case V.null sed of
    True  -> text
    False -> restoreSeds (T.replace (getTlToCmd (V.head sed) 2) (getTlToCmd (V.head sed) 3) text) (V.tail sed)


searchReplyId :: T.Text -> V.Vector GetTL -> [T.Text]
searchReplyId id tl = if T.null id then [] else
 ((\t -> case t of Nothing -> []
                   Just a  -> (gtl_id_str a):(searchReplyId (gtl_id_str a) tl)).V.find ((==id).rpStatus)) tl

rpStatus :: GetTL -> T.Text
rpStatus gtl = case gtl_in_reply_to_status_id_str gtl of
 Nothing -> T.empty
 Just x  -> x

getTweetNextId :: PostQueue -> T.Text
getTweetNextId msg = (T.pack.(show :: Integer -> String).(+(-1)).(read :: String -> Integer).T.unpack) $ getTweetId msg

getTweetId :: PostQueue -> T.Text
getTweetId msg = (snd.T.breakOnEnd (T.singleton '/')) $ (gul_expanded_url.Prelude.head.gen_urls.gmt_entities.V.head.mentions) msg

rmDup :: [T.Text] -> [T.Text]
rmDup = foldl (\seen x -> if x `elem` seen then seen else x:seen) []

filterCmd vmsgq n = ((!! n).Prelude.head.Prelude.map T.words.T.lines.gmt_text.V.head.mentions) vmsgq 

getTlToCmd msg n = ((!! n).Prelude.head.Prelude.map T.words.T.lines) msg

isEqStrText :: String -> T.Text -> Bool
isEqStrText str tx = case (Prelude.null str, T.null tx) of
 (True, True)   -> True
 (False, False) -> if((Prelude.head str) == (T.head tx)) then isEqStrText (Prelude.tail str) (T.tail tx) else False
 otherwise      -> False

splitChar :: T.Text -> (T.Text, Bool, Bool, Bool)
splitChar = ((\[a,b,c,d] -> (a,strToBool b,strToBool c,strToBool d)).commaSep) 

commaSep :: T.Text -> [T.Text]
commaSep text = T.split (==',') text

commaIns :: [T.Text] -> T.Text
commaIns = T.intercalate (T.singleton ',')

strToBool str = if str == T.pack "True" then True else False

userInGroups :: T.Text -> V.Vector (T.Text, V.Vector T.Text) -> T.Text
userInGroups user raw = commaIns.V.toList.V.map fst.V.filter (isInGroup user) $ raw
 where
  isInGroup :: T.Text -> (T.Text, V.Vector T.Text) -> Bool
  isInGroup u g = case V.find (==u) (snd g) of
   Nothing -> False
   Just a  -> True

groupAndUsers :: T.Text -> V.Vector (T.Text, V.Vector T.Text) -> (T.Text, V.Vector T.Text)
groupAndUsers group raw = do
 case V.find ((==group).fst) $ raw of
  Nothing -> (T.empty, V.empty)
  Just gr -> gr

userInGroup :: T.Text -> V.Vector (T.Text, V.Vector T.Text) -> (V.Vector T.Text)
userInGroup group raw = snd $ groupAndUsers group raw

allUsers :: V.Vector (T.Text, V.Vector T.Text) -> V.Vector T.Text
allUsers = userInGroup (T.pack "all")

existInGroup :: T.Text -> T.Text -> V.Vector (T.Text, V.Vector T.Text) -> Bool
existInGroup user group raw = do
 let us = userInGroup group raw 
 case V.find (== user) us of
  Nothing -> False
  Just u  -> True

existUser :: T.Text -> V.Vector (T.Text, V.Vector T.Text) -> Bool
existUser user raw = do
 let all = allUsers raw
 case V.find (== user) all of
  Nothing -> False
  Just u  -> True

addUserInGroup :: T.Text -> T.Text -> V.Vector (T.Text, V.Vector T.Text) -> V.Vector (T.Text, V.Vector T.Text)
addUserInGroup user group raw = case V.elemIndex group ((V.map Prelude.fst) raw) of
 Nothing -> raw
 Just a  -> do
  let fraw = fst.V.splitAt a $ raw
      mraw = snd $ raw V.! a
      lraw = V.tail.snd.V.splitAt a $ raw
  case V.elem user mraw of
   True  -> raw
   False -> fraw V.++ (V.singleton (fst (raw V.! a),V.snoc mraw user)) V.++ lraw

rmUserInGroup :: T.Text -> T.Text -> V.Vector (T.Text, V.Vector T.Text) -> V.Vector (T.Text, V.Vector T.Text)
rmUserInGroup user group raw = case V.elemIndex group ((V.map Prelude.fst) raw) of
  Nothing -> raw
  Just a  -> do
   let fraw = fst.V.splitAt a $ raw
       mraw = V.filter (/=user) $ snd $ raw V.! a
       lraw = V.tail.snd.V.splitAt a $ raw
   fraw V.++ (V.singleton (fst (raw V.! a), mraw)) V.++ lraw

queueToUser :: PostQueue -> T.Text
queueToUser = gur_screen_name.gmt_user.V.head.mentions

addGroup :: T.Text -> V.Vector (T.Text, V.Vector T.Text) -> V.Vector (T.Text, V.Vector T.Text)
addGroup group raw = case V.elemIndex group ((V.map Prelude.fst) raw) of
 Just a  -> raw
 Nothing -> V.snoc raw (group, V.empty)

deleteGroup :: T.Text -> V.Vector (T.Text, V.Vector T.Text) -> V.Vector (T.Text, V.Vector T.Text)
deleteGroup group raw = case V.elemIndex group ((V.map Prelude.fst) raw) of
 Nothing -> raw
 Just a  -> V.filter ((/=group).fst) raw
