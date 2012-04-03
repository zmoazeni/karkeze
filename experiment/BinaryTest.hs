import Data.Binary
import Data.Text.Lazy (pack, unpack, concat)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import Control.Monad (replicateM)
import Data.ByteString.Lazy (ByteString)

type Strings = [String]
data CustomType = CustomType { customStrings :: [String] }
  deriving (Show)

instance Binary CustomType where
  put (CustomType strings) = do put $ length(strings)
                                let encodedStrings = map (encodeUtf8 . pack) strings
                                mapM_ put encodedStrings

  get = do num <- get :: Get Int
           encodedStrings <- replicateM num (get :: Get ByteString)
           let strings = map (unpack . decodeUtf8) encodedStrings
           return (CustomType strings)


main :: IO ()
main = do let encoded = encode $ CustomType ["fo✘⊥o", "bar"]
              decoded = decode encoded :: CustomType
          print encoded
          print decoded
          putStrLn . head $ customStrings decoded
