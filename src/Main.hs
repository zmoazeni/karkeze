import System.Environment (getArgs)
import Parser
import Storage

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("read":rawGram:_)  -> readGram (Gram rawGram)
    ("load":_)          -> loadIndex
    ("grams":_)         -> printGrams
    ("print":_)         -> parseAndPrint
    _                   -> putStrLn "[load|print|grams|read <gram>]"

parseAndPrint :: IO ()
parseAndPrint = do
  rawJsons <- readFile "input.json"
  putStrLn . show $ parseInvertedIndex rawJsons
