{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Web where

import Web.Scotty
import Data.JSON2
import Storage
import Parser
import Data.Text.Lazy (Text, pack)
import Database.LevelDB (DB)
import Control.Monad.IO.Class
import Network.HTTP.Types
import Network.Wai
import "mtl" Control.Monad.Reader
import Data.Conduit.Lazy (lazyConsume)
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Applicative
import Control.Monad.Trans.Resource (ResourceT, runResourceT)

type Param = (Text, Text)

run :: String -> (DB, DB) -> IO ()
run port (db, stageDB) = scotty (read port) $ do
  get "/" $ do
    grams' <- fetchGrams
    text . pack . toString . toJson $ grams'
    header "Content-Type" "application/json"

  post "/" $ do
    -- r <- getBody
    -- liftIO . print $ getBody <$> ask
    -- liftIO $ print r
    r <- request
    let b = BL.fromChunks <$> (lazyConsume . requestBody) r
    liftIO . runResourceT $ printResource b

    v <- param "foo2"
    liftIO $ putStrLn v
    status status201

  where
    fetchGrams :: ActionM [Gram]
    fetchGrams = liftIO (grams db)

    printResource :: ResourceT IO BL.ByteString -> ResourceT IO BL.ByteString
    printResource x = do
      content <- x
      liftIO $ print $ BL.toChunks content
      return content

    -- getBody :: ActionEnv -> BL.ByteString
    -- getBody _ _ b = b
    -- getBody :: ActionM BL.ByteString
    -- getBody =
      -- do r <- request
         -- body <- liftIO . runResourceT $ BL.fromChunks <$> (lazyConsume . requestBody) r
         -- return body
