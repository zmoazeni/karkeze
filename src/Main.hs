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
  let dbPath = databasePath args
  withDB dbPath $ \db ->
    withDB stageDBPath $ \stageDB ->
      case args of
        ("read":rawGram:_)     -> readGram db (Gram rawGram)
        ("load":_)             -> loadIndex db "input.json"
        ("grams":_)            -> printGrams db
        ("rawGrams":_)         -> grams db >>= print
        ("rawKeys":_)          -> keys db >>= print
        ("print":_)            -> parseAndPrint "input.json"
        ("web":port:_)         -> run port (db, stageDB)
        ("flush":_)            -> flush stageDB db
        ("example":_)          -> example db
        ("concurrencytest1":_) -> badConcurrency db
        ("concurrencytest2":_) -> destroy dbPath [] >> separateKeys db
        _                      -> putStrLn "[load|print|grams|read <gram>|web <port>|example|concurrencytest1|concurrencytest2]"

databasePath :: [String] -> FilePath
databasePath ("concurrencytest1":_) = "./db/db-concurrencytest"
databasePath ("concurrencytest2":_) = "./db/db-concurrencytest"
databasePath _                     = "./db/db-leveldbtest"

stageDBPath :: FilePath
stageDBPath = "./db/db-stage"
