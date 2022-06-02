{-|
Module      : Main
Description : The entry-point to KMonad
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module Main
  ( -- * The entry-point to KMonad
    main
  )
where

import qualified KMonad.App as KMonad (main)

import System.IO
import KMonad.Prelude
import KMonad.App.Cfg.Invoc (getInvoc)

-- main :: IO ()
-- main = KMonad.main

main :: IO ()
main = pPrint =<< getInvoc
