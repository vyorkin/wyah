module Wyah.Chapter8.Name
  ( Name(..)
  , prefix
  , unName
  , letters
  , genNames
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad (replicateM)
import Data.String (IsString(..))
import Data.Hashable (Hashable(..))
import GHC.Generics (Generic(..))

data Name
  = Gen  !Text !Integer
  | Name !Text
  deriving (Eq, Ord, Show, Read, Generic)

instance Hashable Name
instance IsString Name where fromString = Name . Text.pack

prefix :: Text -> Name -> Name
prefix p (Gen s n) = Gen  (p <> s) n
prefix p (Name s)  = Name (p <> s)

unName :: IsString a => Name -> a
unName (Name s)  = fromString $ Text.unpack s
unName (Gen s n) = fromString $ Text.unpack s ++ show n

letters :: [Text]
letters = [1..]
  >>= flip replicateM ['a'..'z']
  >>= pure . Text.pack

-- λ> letters & take 30 & drop 24
-- ["y","z","aa","ab","ac","ad"]

genNames :: [Name]
genNames = zipWith Gen letters [0..]
