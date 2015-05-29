{-# LANGUAGE Rank2Types #-}
module Yesod.Contrib.League.Crud.Sort
       ( Sort(..)
       , Sorts(..)
       , SortC
       , ToEntityField
       , cancelLink
       , cancelSort
       , getSorts
       , isSortedBy
       , sortIndicator
       , sortToSelectOpt
       , sortsQuery
       , sortsToSelectOpts
       , toggleLink
       , toggleSort
       ) where

import ClassyPrelude
import Data.String.Utils (split)
import Database.Persist
import Yesod.Contrib.League.Crud
import Yesod.Core
import Safe (toEnumMay)

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

type SortC k = (Eq k, Enum k, Bounded k)

type ToEntityField k f = forall a. k -> (forall t. EntityField f t -> a) -> a

getSorts :: (SortC k, Crud sub) => CrudM sub (Sorts k)
            -- TODO: can I cache this result?
getSorts = decodeSorts . maybe "" unpack <$> lookupGetParam "s"

encodeSorts :: SortC k => Sorts k -> String
encodeSorts = intercalate "." . map (show . fromEnum) . sortsList

decodeSorts :: SortC k => String -> Sorts k
decodeSorts = Sorts . catMaybes . map readEnumMay . split "."

readEnumMay :: SortC k => String -> Maybe k
readEnumMay s = readMay s >>= toEnumMay

sortToSelectOpt :: ToEntityField k f -> Sort k -> SelectOpt f
sortToSelectOpt f (Sort c True) = f c Asc
sortToSelectOpt f (Sort c False) = f c Desc

sortsToSelectOpts :: ToEntityField k f -> Sorts k -> [SelectOpt f]
sortsToSelectOpts f = map (sortToSelectOpt f) . sortsList

sortsQuery :: SortC k => Sorts k -> [(Text, Text)]
sortsQuery s =
  if null e then [] else [("s", pack e)]
  where e = encodeSorts s

toggleSort :: SortC k => k -> Bool -> Sorts k -> Sorts k
toggleSort k f s =
  case isSortedBy k s of
   Nothing  -> liftSorts (Sort k f :) $ cancelSort k s
   Just (_, asc) -> liftSorts (Sort k (not asc) :) $ cancelSort k s

sortEq :: SortC k => k -> Sort k -> Bool
sortEq k (Sort j _) = j == k

cancelSort :: SortC k => k -> Sorts k -> Sorts k
cancelSort k = liftSorts $ filter (not . sortEq k)

liftSorts :: ([Sort j] -> [Sort k]) -> Sorts j -> Sorts k
liftSorts f = Sorts . f . sortsList

isSortedBy :: SortC k => k -> Sorts k -> Maybe (Int, Bool)
isSortedBy k = loop 1 . sortsList
  where loop _ [] = Nothing
        loop i (Sort j asc : _) | j == k = Just (i, asc)
        loop i (_: s) = loop (i+1) s

toggleLink
  :: (SortC k, Crud sub)
     => k
     -> Bool
     -> Sorts k
     -> (Route (CrudSubsite sub) -> Route (Site sub))
     -> (Route (Site sub), [(Text, Text)])

toggleLink criterion defaultAscending sorts r =
  (r CrudListR, sortsQuery (toggleSort criterion defaultAscending sorts))

cancelLink
  :: (SortC k, Crud sub)
     => k
     -> Sorts k
     -> (Route (CrudSubsite sub) -> Route (Site sub))
     -> (Route (Site sub), [(Text, Text)])

cancelLink criterion sorts r =
  (r CrudListR, sortsQuery (cancelSort criterion sorts))

sortIndicator
  :: (SortC k, Crud sub)
     => k
     -> Sorts k
     -> (Route (CrudSubsite sub) -> Route (Site sub))
     -> CrudWidget sub

sortIndicator criterion sorts r =
  [whamlet|
   $maybe (i,k) <- isSortedBy criterion sorts
     <span .crud-sorts>
       $if k
         ↑
       $else
         ↓
       #{i}
       <a href=@?{cancelLink criterion sorts r}>×
   |]
