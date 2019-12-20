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

type GandU = (T.Text, V.Vector T.Text) -- (group, users)
type Func = Postfunc -> BotsAPI -> IO(V.Vector (T.Text, ZonedTime), V.Vector GandU)

-- have to MVector
data PostQueue = PostQueue { mentions :: V.Vector GetMention
                           , schedule :: V.Vector (T.Text, ZonedTime)-- twid, retweet time
                           , pqGroups :: V.Vector GandU
                           } deriving (Show)

data Lex = Lex { subcmd          :: T.Text
               , group           :: T.Text -- :<name>
               , users           :: V.Vector T.Text -- @<name>
               , first_id        :: (T.Text, Bool) -- mode :: True -> &since_id= False -> &max_id=
               , lex_screen_name :: T.Text -- who send command
               , lex_user_id     :: T.Text -- id which is sender
               } deriving (Show)

data Week = Monday
          | Tuesday
          | Wednesday
          | Thursday
          | Friday
          | Saturday
          | Sunday
          deriving (Show, Enum, Eq)

data BotsAPI = BotsAPI { twitter   :: [String]
                       , slack     :: [String]
                       , discord   :: String}

data Postfunc = Postfunc { tl       :: T.Text -> T.Text -> BotsAPI -> IO(T.Text)
                         , dm       :: T.Text -> T.Text -> BotsAPI -> IO(T.Text)}

lexAllNull = Lex { subcmd = T.empty
                 , group  = T.empty
                 , users  = V.empty
                 , first_id = (T.empty, True)
                 , lex_screen_name = T.empty
                 , lex_user_id = T.empty }

getUserAllNULL = User { gur_id_str = T.empty
                      , gur_screen_name = T.empty}

getTLAllNULL = GetTL { gtl_text = T.empty
                     , gtl_id_str = T.empty
                     , gtl_in_reply_to_status_id_str = Nothing
                     , gtl_user = getUserAllNULL }

temp = T.empty
vemp = V.empty

botscreen_name = "calc_mie"
helpFile = "/usr/local/calc-tweet/helps/help.txt" 
twHelpFile = "/usr/local/calc-tweet/helps/tweet_help.txt"
uHelpFile = "/usr/local/calc-tweet/helps/users_help.txt"
gHelpFile = "/usr/local/calc-tweet/helps/groups_help.txt"
groupsconf = "/usr/local/calc-tweet/groups.conf"

emptyint = 1*1000*1000  :: Int {- 1 second -} 
mentiont = 12*1000*1000 :: Int {-12 second -}


lexAnalyser :: GetMention -> Either T.Text Lex
lexAnalyser gmt = case ((lexTextAnalyser.parse) gmt, gmt_in_reply_to_status_id_str gmt, (gen_urls.gmt_entities) gmt) of
 (Left t, _, _ )        -> Left t
 (Right l, Nothing, [])  -> Right $ l { lex_screen_name = gmtToSN gmt, lex_user_id = gmtToUI gmt}
 (Right l, Just a, [])   -> Right $ l { first_id = (a, False), lex_screen_name = gmtToSN gmt, lex_user_id = gmtToUI gmt}
 (Right l, Nothing, [a]) -> Right $ l { first_id = (gulToTweetId a, True), lex_screen_name = gmtToSN gmt, lex_user_id = gmtToUI gmt}
 (Right l, Just a, x)    -> Left $ T.pack "double post target."
 _                       -> Left $ T.pack "....."
 where
  lexTextAnalyser :: [T.Text] -> Either T.Text Lex
  lexTextAnalyser []     = Right lexAllNull
  lexTextAnalyser (x:xs) = case lexTextAnalyser xs of
   Left  err -> Left err
   Right lan -> case T.head x of
    ':' -> if (T.null.group) lan then Right lan { group = T.tail x } else Left $ analyGroupsError x (group lan)
    '@' -> Right lan { users =  V.cons (T.tail x) (users lan) }
    _   -> if (T.null.subcmd) lan then Right lan { subcmd = x } else Left $ analySubcmdError x (subcmd lan)
  analyGroupsError :: T.Text -> T.Text -> T.Text
  analyGroupsError first second = T.append (T.pack "double groups selects : ") $  T.append first $ T.append (T.pack " and ") second
  analySubcmdError :: T.Text -> T.Text -> T.Text
  analySubcmdError first second = T.append (T.pack "double sub command selects : ") $ T.append first $ T.append (T.pack " and ") second
  gulToTweetId :: GetUrls -> T.Text
  gulToTweetId = (snd.T.breakOnEnd (T.singleton '/')).gul_expanded_url
  parse :: GetMention -> [T.Text]
  parse = Prelude.tail.T.words.Prelude.head.T.lines.gmt_text

-- bug??
restoreSeds :: T.Text -> V.Vector T.Text -> T.Text
restoreSeds text sed = case V.null sed of
 True  -> text
 False -> restoreSeds (T.replace (getTlToCmd (V.head sed) 2) (getTlToCmd (V.head sed) 3) text) (V.tail sed)
   
rtSearchReplyTree :: GetTL -> V.Vector GetTL -> T.Text -- create all message
rtSearchReplyTree tl tls = if T.null (gtl_id_str tl) then T.empty else do
 let replys = V.filter ((==gtl_id_str tl).rpStatus) tls
     (seds, next) = sedsNext replys
 T.append (restoreSeds (gtl_text tl) (V.map gtl_text seds)) $ T.append (T.singleton '\n') $ rtSearchReplyTree next tls
  where
   sedsNext :: V.Vector GetTL -> (V.Vector GetTL, GetTL)
   sedsNext rep = case V.find (not.isEqStrText "calc-tweet".(`getTlToCmd` 0).gtl_text) rep of
    Nothing -> (V.filter (isEqStrText "calc-tweet".(`getTlToCmd` 0).gtl_text) rep, getTLAllNULL)
    Just a  -> (V.filter (isEqStrText "calc-tweet".(`getTlToCmd` 0).gtl_text) rep, a)

repSearchReplyTree :: GetTL -> V.Vector GetTL -> T.Text
repSearchReplyTree tl tls = do
 let replys = V.filter ((==(rpStatus tl)).gtl_id_str) tls
     seds   = V.filter (\x -> check 0 x && check 1 x) replys
 case gtl_in_reply_to_status_id_str tl of
  Nothing -> restoreSeds (gtl_text tl) (V.map gtl_text seds)
  Just x  -> T.append (repSearchReplyTree (statusToGl x tls) tls) $ restoreSeds (gtl_text tl) (V.map gtl_text seds)
 where
  check :: Int -> GetTL -> Bool 
  check 0 = isEqStrText "calc-tweet".(`getTlToCmd` 0).gtl_text
  check 1 = isEqStrText "sed".(`getTlToCmd` 1).gtl_text

searchReplyId :: T.Text -> V.Vector GetTL -> [T.Text]
searchReplyId id tl = if T.null id then [] else
 ((\t -> case t of Nothing -> []
                   Just a  -> (gtl_id_str a):(searchReplyId (gtl_id_str a) tl)).V.find ((==id).rpStatus)) tl

rpStatus :: GetTL -> T.Text
rpStatus gtl = case gtl_in_reply_to_status_id_str gtl of
 Nothing -> T.empty
 Just x  -> x

statusToGl :: T.Text -> V.Vector GetTL -> GetTL
statusToGl id gtl = case V.find ((==id).gtl_id_str) gtl of
 Nothing -> getTLAllNULL
 Just x  -> x

gmtToRpStatus :: GetMention -> T.Text
gmtToRpStatus gmt = case gmt_in_reply_to_status_id_str gmt of
 Nothing -> T.empty
 Just x  -> x

getTweetId :: GetMention -> T.Text
getTweetId gmt = (snd.T.breakOnEnd (T.singleton '/')) $ (gul_expanded_url.Prelude.head.gen_urls.gmt_entities) gmt

rmDup :: [T.Text] -> [T.Text]
rmDup = foldl (\seen x -> if x `elem` seen then seen else x:seen) []

vectRmDup :: V.Vector T.Text -> V.Vector T.Text
vectRmDup = V.foldl (\seen x -> if x `V.elem` seen then seen else V.cons x seen) V.empty

splitGroupUsers :: [T.Text] -> (T.Text, V.Vector T.Text)
splitGroupUsers = (\(a,b) -> (a, (V.fromList.rmDup) b)).(`ssgu` (T.empty, []))
 where
  ssgu :: [T.Text] -> (T.Text, [T.Text]) -> (T.Text, [T.Text]) -- split select group users, O(n)
  ssgu []     (group, users) = (group, users)
  ssgu (x:xs) (group, users) = if '@' == (T.head x) then (group, (T.tail x):users) else (x, users)

gtlToVector :: Either String [GetTL] -> V.Vector GetTL
gtlToVector gtl = (\a -> case a of Left e  -> V.empty
                                   Right r -> (V.reverse.V.fromList) r) gtl

getFirstGtl :: T.Text -> V.Vector GetTL -> GetTL
getFirstGtl fid = (\a -> case a of Nothing -> getTLAllNULL
                                   Just r  -> r).V.find ((==fid).gtl_id_str)
   

-- filterCmd vmsgq n = ((!! n).Prelude.head.Prelude.map T.words.T.lines.gmt_text.V.head.mentions) vmsgq 
-- getTlToCmd msg n = ((!! n).Prelude.head.Prelude.map T.words.T.lines) msg

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

-- return group
groupInUser :: T.Text -> V.Vector (T.Text, V.Vector T.Text) -> T.Text
groupInUser user raw = commaIns.V.toList.V.map fst.V.filter (isInGroup user) $ raw
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

-- return user
usersInGroup :: T.Text -> V.Vector (T.Text, V.Vector T.Text) -> V.Vector T.Text
usersInGroup group raw = snd $ groupAndUsers group raw

existInGroup :: T.Text -> T.Text -> V.Vector (T.Text, V.Vector T.Text) -> Bool
existInGroup user group raw = do
 let us = usersInGroup group raw 
 case V.find (== user) us of
  Nothing -> False
  Just u  -> True

existUser :: T.Text -> V.Vector (T.Text, V.Vector T.Text) -> Bool
existUser user raw = (V.or.V.map (V.elem user).(V.map snd)) raw

addUsersInGroup :: V.Vector T.Text -> T.Text -> V.Vector (T.Text, V.Vector T.Text) -> V.Vector (T.Text, V.Vector T.Text)
addUsersInGroup user group raw = if V.null user then raw else do
 let next = addUsersInGroup (V.tail user) group raw
 case V.elemIndex group ((V.map Prelude.fst) next) of
  Nothing -> next
  Just a  -> do
   let fraw = fst.V.splitAt a $ next
       mraw = snd $ next V.! a
       lraw = V.tail.snd.V.splitAt a $ next
   case V.elem (V.head user) mraw of
    True  -> next
    False -> fraw V.++ (V.singleton (fst (next V.! a),V.snoc mraw (V.head user))) V.++ lraw

rmUsersInGroup :: V.Vector T.Text -> T.Text -> V.Vector (T.Text, V.Vector T.Text) -> V.Vector (T.Text, V.Vector T.Text)
rmUsersInGroup user group raw = if V.null user then raw else do
 let next = rmUsersInGroup (V.tail user) group raw
 case V.elemIndex group ((V.map Prelude.fst) next) of
  Nothing -> next
  Just a  -> do
   let fnext = fst.V.splitAt a $ next
       mnext = V.filter (/=(V.head user)) $ snd $ next V.! a
       lnext = V.tail.snd.V.splitAt a $ next
   fnext V.++ (V.singleton (fst (next V.! a), mnext)) V.++ lnext

--queueToUser :: PostQueue -> T.Text
--queueToUser = gur_screen_name.gmt_user.V.head.mentions

gmtToSN :: GetMention -> T.Text
gmtToSN = gur_screen_name.gmt_user

gmtToUI :: GetMention -> T.Text
gmtToUI = gur_id_str.gmt_user

addGroup :: T.Text -> V.Vector (T.Text, V.Vector T.Text) -> V.Vector (T.Text, V.Vector T.Text)
addGroup group raw = case V.elemIndex group ((V.map Prelude.fst) raw) of
 Just a  -> raw
 Nothing -> V.snoc raw (group, V.empty)

deleteGroup :: T.Text -> V.Vector (T.Text, V.Vector T.Text) -> V.Vector (T.Text, V.Vector T.Text)
deleteGroup group raw = case V.elemIndex group ((V.map Prelude.fst) raw) of
 Nothing -> raw
 Just a  -> V.filter ((/=group).fst) raw
