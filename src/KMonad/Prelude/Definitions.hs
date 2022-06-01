{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |

module KMonad.Prelude.Definitions
  ( Name

  , Dt
  , us
  , ms
  )
where

import KMonad.Prelude.Imports

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
