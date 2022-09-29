module Demo where
import MonadTrans
import ReaderT
import WriterT
import Control.Monad.Identity
import Data.Char (toUpper)


strings = ["abc", "defg", "hij"]

logFirstAndRetSecond :: ReaderT [String] (WriterT String Identity) String
logFirstAndRetSecond = do
  el1 <- asks head
  el2 <- asks (map toUpper . head . tail)
  ReaderT.lift $ tell el1
  return el2
