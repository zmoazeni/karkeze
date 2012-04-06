{-# LANGUAGE OverloadedStrings #-}

module Web where

import Web.Scotty
import Control.Monad.IO.Class
import Data.Text.Lazy as L
import Data.Text.Lazy.Encoding
import Data.ByteString.Lazy.Char8 as C (unpack)

main :: IO ()
main = scotty 3002 $ do
  post "/" $ do
    -- b <- body
    -- f <- param "foo" :: ActionM Text
    -- liftIO $ putStrLn (unpack f)
    -- f <- param "foo" :: ActionM String
    -- liftIO $ putStrLn f

    -- b <- body
    -- liftIO $ print b

    -- b <- body
    -- liftIO $ putStrLn (C.unpack b) -- bad encoding

    b <- body
    liftIO $ print (C.unpack b) -- bad encoding
    liftIO $ print (decodeUtf8 $ b) -- good encoding
    liftIO $ putStrLn (L.unpack . decodeUtf8 $ b) -- good encoding
    text "ok"

