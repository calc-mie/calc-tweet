{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main(main) where

import Lib
import TwitterAPI
import qualified Data.Text.IO as T
import Data.Text
import System.Directory

main = do
 -- calcweb-post
 oldcalcweb <- getDirectoryContents srvcalcdir
 -- main
 direct_message <- getGetDM
 case direct_message of
  Right dm -> monitoring (setPostData ([],oldcalcweb,[])) dm >> putStrLn "fin"
