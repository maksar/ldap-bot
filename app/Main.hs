module Main where

import           Main.Utf8
import           Data.Text

import           App

main :: IO ()
main = withUtf8 $ runBot bot >>= either (putStrLn . unpack) return
