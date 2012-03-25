import System.Environment (getArgs)
import Parser
import Storage
import Web
import CLI
import ConcurrencyTest
import Database.LevelDB

main :: IO ()
main = do
  args <- getArgs
  withDB (databasePath args) $ \db ->
    case args of
      ("read":rawGram:_)     -> readGram db (Gram rawGram)
      ("load":_)             -> loadIndex db "input.json"
      ("grams":_)            -> printGrams db
      ("rawGrams":_)         -> grams db >>= print
      ("rawKeys":_)          -> withIterator db [] (\iter -> do { iterFirst iter; keys iter >>= print})
      ("print":_)            -> parseAndPrint "input.json"
      ("web":port:_)         -> run port db
      ("example":_)          -> example db
      ("concurrencytest1":_) -> badConcurrency db
      _                      -> putStrLn "[load|print|grams|read <gram>|web <port>|example|concurrencytest1]"

databasePath :: [String] -> FilePath
databasePath ("concurrencytest":_) = "./db/db-concurrencytest"
databasePath _                     = "./db/leveldbtest"
