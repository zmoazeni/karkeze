import System.Environment (getArgs)
import Parser
import Storage
import Web

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("read":rawGram:_)  -> readGram (Gram rawGram)
    ("load":_)          -> loadIndex
    ("grams":_)         -> printGrams
    ("print":_)         -> parseAndPrint
    ("web":port:_)      -> withDB $ \db -> run port db
    _                   -> putStrLn "[load|print|grams|read <gram>|web <port>]"

parseAndPrint :: IO ()
parseAndPrint = do
  rawJsons <- readFile "input.json"
  putStrLn . show $ parseInvertedIndex rawJsons
