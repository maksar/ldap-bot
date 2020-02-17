module Main where

import           Data.Text

import           App

main :: IO ()
main = runBot bot >>= either (putStrLn . unpack) return
