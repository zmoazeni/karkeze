{-# LANGUAGE OverloadedStrings #-}

import Parser
import Data.Text
import Data.Aeson as A
import Data.Binary as B
import Data.Vector as V
import Data.Maybe

main :: IO ()
main = do let binaryJson = A.encode $ fromList [Number 1]
          -- let binaryJson = A.encode $ object [("foo", Number 1)]
          print binaryJson
          let regJson = A.decode binaryJson :: Maybe Value
          print regJson
          let (Just (Array v)) = regJson
          print $ V.head v
