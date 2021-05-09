{-# LANGUAGE OverloadedStrings #-}

module Asana (
    getTasksForProject
  , getSubtasks
  , getDependencies
  , Task (..)
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
instance ToJSON Task where
  toJSON (Task id name) = object [ "gid" .= id, "name" .= name ]

newtype AsanaResponse = AsanaResponse { asanaResponseData :: [Task] }

instance FromJSON AsanaResponse where
  parseJSON (Object v) = AsanaResponse <$> v .: "data"

getFromAsana :: String -> String -> IO [Task]
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

getTasksForProject :: String -> String -> IO [Task]
getTasksForProject token projectId = getFromAsana token $ "projects/" ++ projectId ++ "/tasks"

getSubtasks :: String -> String -> IO [Task]
getSubtasks token taskId = getFromAsana token $ "tasks/" ++ taskId ++ "/subtasks"

getDependencies :: String -> String -> IO [Task]
getDependencies token taskId = getFromAsana token $ "tasks/" ++ taskId ++ "/dependencies"
