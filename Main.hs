import System.IO
import Parser
import Data.JSON2 (Json(..))

import Database.LevelDB
import Data.Text as T
import Data.Text.Encoding as E
import Data.ByteString (ByteString)
import qualified Data.Serialize as S (encode, decode)

data Encodable = EString String | EGram Gram | EJson Json
data Decodable = ToDString ByteString | ToDGram ByteString
data Decoded = DString String | DGram Gram
  deriving (Show)

main :: IO ()
main = do
  rawJsons <- readFile "input.json"
  putStrLn . show $ parseInvertedIndex rawJsons

  withLevelDB "./db/leveldbtest" [CreateIfMissing, CacheSize 1024] $ \db -> do
    put db [] (encode $ EString "foo") (encode $ EString "bar≠")
    Just val <- get db [] (encode $ EString "foo")
    let DString valString = decode $ ToDString val in
      putStrLn valString

  where 
    encode (EString string) = E.encodeUtf8 $ T.pack string
    encode (EGram (Gram string)) = encode $ EString string
    encode (EJson (JNumber i)) = S.encode i
    
    decode :: Decodable -> Decoded
    decode (ToDString byteString) = DString . T.unpack $ E.decodeUtf8 byteString
    decode (ToDGram byteString) = let DString string = decode $ ToDString byteString in DGram (Gram string)
    