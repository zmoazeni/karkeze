{-# LANGUAGE OverloadedStrings #-}
module Web where

import Web.Scotty
import Data.JSON2
import Storage
import Parser
import Control.Monad.Trans
import Data.Text.Lazy (pack)
import Database.LevelDB (DB)

run :: String -> DB -> IO ()
run port db = scotty port' $ do
  get "/grams" $ do
    grams' <- fetchGrams
    text . pack . toString . toJson $ grams'
    header "Content-Type" "application/json"
  where 
    fetchGrams :: ActionM [Gram]
    fetchGrams = liftIO (grams db)

    port' :: Int
    port' = read port

 -- get "/hello" $ do
   -- text "[{\"foo\":1}]"
   -- header "Content-Type" "application/json"
