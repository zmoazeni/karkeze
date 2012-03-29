{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Web where

import Web.Scotty
import Data.JSON2
import Storage
import Parser
import Data.Text.Lazy (pack)
import Database.LevelDB (DB)
import Control.Monad.IO.Class
import Network.HTTP.Types

run :: String -> (DB, DB) -> IO ()
run port (db, stageDB) = scotty (read port) $ do
  get "/" $ do
    grams' <- fetchGrams
    text . pack . toString . toJson $ grams'
    header "Content-Type" "application/json"

  post "/" $ do
    b <- body
    liftIO $ saveAction stageDB IndexCreate b
    status status201

  where
    fetchGrams :: ActionM [Gram]
    fetchGrams = liftIO (grams db)
