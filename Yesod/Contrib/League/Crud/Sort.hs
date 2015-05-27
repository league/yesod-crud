module Yesod.Contrib.League.Crud.Sort where

import ClassyPrelude
import Data.String.Utils (split)
import Yesod.Contrib.League.Crud
import Yesod.Core

-- could be added to Safe library
toEnumMay :: (Enum a, Bounded a) => Int -> Maybe a
toEnumMay i =
  let r = toEnum i
      mx = maxBound `asTypeOf` r
      mn = minBound `asTypeOf` r
  in if i >= fromEnum mn && i <= fromEnum mx
  then Just r
  else Nothing

data Sort k =
  Sort { sortKey :: k
       , sortAsc :: Bool
       }
  deriving (Eq, Show)

-- Assumes that Enum k produces non-negative int, so we can encode the up/down
-- flag as the sign.
instance Enum k => Enum (Sort k) where
  fromEnum (Sort k True) = fromEnum k + 1
  fromEnum (Sort k False) = - (fromEnum k + 1)
  toEnum i = Sort (toEnum (abs i - 1)) (i > 0)

instance Bounded k => Bounded (Sort k) where
  minBound = Sort maxBound False
  maxBound = Sort maxBound True

newtype Sorts k =
  Sorts { sortsList :: [Sort k] }
  deriving (Eq, Show)

getSorts :: Crud sub => CrudM sub (Sorts (SortC sub))
getSorts = decodeSorts . maybe "" unpack <$> lookupGetParam "s"

decodeSorts :: (Enum k, Bounded k) => String -> Sorts k
decodeSorts = Sorts . catMaybes . map readEnumMay . split "."

readEnumMay :: (Enum k, Bounded k) => String -> Maybe k
readEnumMay s = readMay s >>= toEnumMay
