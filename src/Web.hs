{-# LANGUAGE OverloadedStrings #-}

module Web (run) where

import Web.Scotty
import Storage
import Parser
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Database.LevelDB (DB)
import Control.Monad
import Control.Monad.IO.Class
import Network.HTTP.Types
import Data.Aeson

run :: String -> (DB, DB) -> IO ()
run port (gramDB, stageDB) = scotty (read port) $ do
  get "/grams" $ do
    grams' <- fetchGrams gramDB
    text . decodeUtf8 . encode . toJSON $ grams'
    header "Content-Type" "application/json"

  get "/search" $ do
    query <- param "q"
    fields <- liftM toFields $ param "f" `rescue` (\_ -> return (pack ""))
    results <- liftIO $ search gramDB query fields
    text . decodeUtf8 . encode $ results
    header "Content-Type" "application/json"

  post "/" $ do
    b <- body
    liftIO $ queueAction stageDB IndexCreate b
    status status201

fetchGrams :: DB -> ActionM [Gram]
fetchGrams db = liftIO (grams db)

toFields :: Text -> [Text]
toFields "" = [] 
toFields combined = splitOn "," combined