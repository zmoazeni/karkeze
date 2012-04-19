{-# LANGUAGE OverloadedStrings #-}

module Web (run) where

import Web.Scotty
import Storage
import Parser
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Control.Monad
import Control.Monad.IO.Class
import Network.HTTP.Types
import Data.Aeson

run :: String -> Databases -> IO ()
run port dbs@(Databases {gramDB=gramDB', stageDB=stageDB'}) = scotty (read port) $ do
  get "/grams" $ do
    grams' <- fetchGrams dbs
    text . decodeUtf8 . encode . toJSON $ grams'
    header "Content-Type" "application/json"

  get "/search" $ do
    query <- param "q"
    fields <- liftM toFields $ param "f" `rescue` (\_ -> return "")
    results <- liftIO $ search gramDB' query fields
    text . decodeUtf8 . encode $ results
    header "Content-Type" "application/json"

  post "/" $ do
    b <- body
    liftIO $ queueAction stageDB' IndexCreate b
    status status201
    
  delete "/" $ do
    b <- body
    liftIO $ queueAction stageDB' IndexDelete b
    status status200
    
  put "/" $ do
    b <- body
    liftIO $ queueAction stageDB' IndexUpdate b
    status status200

fetchGrams :: Databases -> ActionM [Gram]
fetchGrams dbs = liftIO (grams dbs)

toFields :: Text -> [Text]
toFields "" = [] 
toFields combined = splitOn "," combined