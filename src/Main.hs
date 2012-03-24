import System.Environment (getArgs)
import Parser
import Storage
import Web
import CLI

main :: IO ()
main = do
  args <- getArgs
  withDB $ \db ->
    case args of
      ("read":rawGram:_)  -> readGram db (Gram rawGram)
      ("load":_)          -> loadIndex db "input.json"
      ("grams":_)         -> printGrams db
      ("print":_)         -> parseAndPrint "input.json"
      ("web":port:_)      -> run port db
      ("example":_)       -> example db
      _                   -> putStrLn "[load|print|grams|read <gram>|web <port>|example]"
