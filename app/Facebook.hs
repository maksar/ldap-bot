module Main where

import App (botFacebook, runBot)
import Data.Text (unpack)
import Main.Utf8 (withUtf8)

main :: IO ()
main = withUtf8 $ runBot botFacebook >>= either (putStrLn . unpack) return
