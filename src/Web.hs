{-# LANGUAGE OverloadedStrings #-}
module Web where

import Web.Scotty
import Data.JSON2
import Storage
import Parser
import Control.Monad.Trans
import Data.Text.Lazy (pack)

run :: IO ()
run = scotty 3000 $ do
  get "/grams" $ do
    g <- theGrams
    text . pack . toString . toJson $ g
    header "Content-Type" "application/json"
  where 
    theGrams :: ActionM [Gram]
    theGrams = liftIO grams

 -- get "/hello" $ do
   -- text "[{\"foo\":1}]"
   -- header "Content-Type" "application/json"
