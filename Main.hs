import System.IO
import Parser

main :: IO ()
main = do
  rawJsons <- readFile "input.json"
  putStrLn . show $ parseInvertedIndex rawJsons
    