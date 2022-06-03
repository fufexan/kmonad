{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |

module KMonad.Prelude.Definitions
  ( Name

  , Dt
  , us
  , ms

  , duplicates
  )
where

import KMonad.Prelude.Imports

import qualified RIO.List as L

--------------------------------------------------------------------------------

-- | Text in its function as a symbol for some other value
type Name = Text

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
duplicates :: Eq a => [a] -> [a]
duplicates l = (L.\\) l $ L.nub l
