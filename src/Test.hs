{-# LANGUAGE OverloadedStrings #-}
module Test where

import Web.Scotty

run :: IO ()
run = scotty 3000 $ do
  get "/hello" $ do
    text "[{\"foo\":1}]"
    header "Content-Type" "application/json"
