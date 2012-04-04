import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as SC
import qualified Data.ByteString.Lazy.Char8 as LC

main :: IO ()
main = do let strict = SC.pack "foobar"
              lazy = L.fromChunks [strict]
          print $ LC.unpack lazy
