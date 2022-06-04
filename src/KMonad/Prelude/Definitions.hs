{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |

module KMonad.Prelude.Definitions
  ( Name
  , Names
  , Named
  , NameError
  , HasName(..)
  , HasNames(..)
  , AsNameError(..)
  , validate
  , validated

  , Dt
  , us
  , ms

  , duplicates
  )
where

import KMonad.Prelude.Imports

import qualified RIO.List     as L
import qualified RIO.Text     as T



--------------------------------------------------------------------------------

-- | A duration of time encoded as some non-negative amount of microseconds
newtype Dt = Dt { _us :: Natural }
  deriving (Num, Eq, Ord, Show, Read, Generic)
makeLenses ''Dt

-- | A lens between a non-negative amount of milliseconds and 'Dt'
ms :: Iso' Dt Natural
ms = iso (view $ us . to (`div` 1000)) (Dt . (* 1000))


--------------------------------------------------------------------------------

-- | Return a list of duplicate elements
--
-- This could be faster but is never really used for time-critical or large tasks.
duplicates :: Eq a => [a] -> Maybe [a]
duplicates l = case (L.\\) l $ L.nub l of
  [] -> Nothing
  x  -> Just x

--------------------------------------------------------------------------------

-- | Text in its function as a symbol for some other value
type Name = Text
type Names = [Name]
type Named a = [(Name, a)]

-- | Things that can go wrong with 'Name' or 'Names'
data NameError
  = EmptyName            -- ^ Encountered an empty 'Name'
  | DuplicateNames Names -- ^ Encountered duplicate 'Names'
  deriving Eq
makeClassyPrisms ''NameError

instance Show NameError where
  show EmptyName = "Encountered an empty <Name>"
  show (DuplicateNames ns) = "Encountered duplicate names: "
    <> (unpack . T.intercalate ", " . map tshow $ ns)

instance Exception NameError
instance AsNameError SomeException where _NameError = exception

class HasName a where name :: Lens' a Name
class HasNames a where names :: Fold a Name

instance HasName Name where name = id
instance HasNames Name where names = id
instance HasNames Names where names = folded
instance HasNames (Named a) where names = folded . _1

-- | See if a collection of names is valid
validate :: HasNames a => a -> Maybe NameError
validate a = let ns = a^..names in case L.find T.null ns of
  Just _  -> Just EmptyName
  Nothing -> DuplicateNames <$> duplicates ns

-- | Check a collection of names for validity and when lacking, throw an error
validated :: HasNames a => a -> a
validated x = maybe x (throwing _NameError) . validate $ x
