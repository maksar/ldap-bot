module Main where

import App (bot, runBot)
import Data.Text (unpack)
import Main.Utf8 (withUtf8)

main :: IO ()
main = withUtf8 $ runBot bot >>= either (putStrLn . unpack) return
