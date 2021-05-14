{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Yesod
import Data.Aeson (Value)
import TaskGraph (fetchTaskTree, merge)
import Asana (getTasksForProject)
import System.Environment (getEnv)

data GrasanaApp = GrasanaApp

mkYesod "GrasanaApp" [parseRoutes|
                     / HomeR GET
                     /projects/#Int ProjectR GET
                     |]

instance Yesod GrasanaApp

getHomeR = return $ object ["msg" .= "Hello World"]

getProjectR :: Int -> Handler Value
getProjectR projectIdInt = do
  token <- liftIO $ getEnv "ASANA_PAT"
  tasks <- liftIO $ getTasksForProject token $ show projectIdInt
  returnJson =<< liftIO (foldr merge ([], []) <$> mapM (fetchTaskTree token) tasks)

main = warp 1337 GrasanaApp
