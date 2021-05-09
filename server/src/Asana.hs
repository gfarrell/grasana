{-# LANGUAGE OverloadedStrings #-}

module Asana (
    getTasksForProject
  , getSubtasks
  , getDependencies
  , Task
  , ApiResponse
  ) where

import Data.Aeson
import Data.ByteString.UTF8 (fromString)
import Network.HTTP.Simple

data Task = Task {
     taskId :: String
  ,  taskName :: String
  } deriving (Show)

instance FromJSON Task where
  parseJSON (Object v) = Task <$> v .: "gid" <*> v .: "name"

newtype AsanaResponse = AsanaResponse { asanaResponseData :: [Task] }

instance FromJSON AsanaResponse where
  parseJSON (Object v) = AsanaResponse <$> v .: "data"

type ApiResponse = [Task]

getFromAsana :: String -> String -> IO ApiResponse
getFromAsana token path = do
  response <- httpJSON
              $ addRequestHeader "Authorization" (fromString $ "Bearer " ++ token)
              $ setRequestSecure True
              $ setRequestPort 443
              $ setRequestHost "app.asana.com"
              $ setRequestPath (fromString $ "/api/1.0/" ++ path)
              $ setRequestMethod "GET"
              defaultRequest
  return . asanaResponseData . getResponseBody $ response

getTasksForProject :: String -> String -> IO ApiResponse
getTasksForProject token projectId = getFromAsana token $ "projects/" ++ projectId ++ "/tasks"

getSubtasks :: String -> String -> IO ApiResponse
getSubtasks token taskId = getFromAsana token $ "tasks/" ++ taskId ++ "/subtasks"

getDependencies :: String -> String -> IO ApiResponse
getDependencies token taskId = getFromAsana token $ "tasks/" ++ taskId ++ "/dependencies"
