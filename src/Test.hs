{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  get "/hello" $ do
    text "[{\"foo\":1}]"
    header "Content-Type" "application/json"
