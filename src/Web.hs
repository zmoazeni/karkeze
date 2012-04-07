{-# LANGUAGE OverloadedStrings #-}

module Web where

import Web.Scotty
import Storage
import Parser
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Database.LevelDB (DB)
import Control.Monad.IO.Class
import Network.HTTP.Types
import Data.Aeson

run :: String -> (DB, DB) -> IO ()
run port (gramDB, stageDB) = scotty (read port) $ do
  get "/grams" $ do
    grams' <- fetchGrams
    text . decodeUtf8 . encode . toJSON $ grams'
    header "Content-Type" "application/json"

  get "/search" $ do
    q <- param "q"
    let gram = Gram (toStrict q)
    results <- liftIO $ search gramDB gram
    text . decodeUtf8 . encode $ results
    header "Content-Type" "application/json"

  post "/" $ do
    b <- body
    liftIO $ queueAction stageDB IndexCreate b
    status status201

  where
    fetchGrams :: ActionM [Gram]
    fetchGrams = liftIO (grams gramDB)
