{-# LANGUAGE OverloadedStrings #-}

module Web where

import Web.Scotty
import Data.JSON2
import Storage
import Parser
import Data.Text.Lazy (pack)
import Database.LevelDB (DB)
import Control.Monad.IO.Class
import Network.HTTP.Types
import Data.ByteString.Char8 (unpack)

run :: String -> (DB, DB) -> IO ()
run port (gramDB, stageDB) = scotty (read port) $ do
  get "/grams" $ do
    grams' <- fetchGrams
    text . pack . toString . toJson $ grams'
    header "Content-Type" "application/json"

  get "/search" $ do
    q <- param "q"
    let gram = Gram (unpack q)
    results <- liftIO $ search gramDB gram
    text . pack . toString $ results
    header "Content-Type" "application/json"

  post "/" $ do
    b <- body
    liftIO $ queueAction stageDB IndexCreate b
    status status201

  where
    fetchGrams :: ActionM [Gram]
    fetchGrams = liftIO (grams gramDB)
