module Main where

import           Control.Monad.Freer       ( runM )
import           Control.Monad.Freer.Error ( runError )

import           Data.Text                 ( unpack )

import           App

main :: IO ()
main = (runM . runError) ldabot >>= either (putStrLn . unpack) return
