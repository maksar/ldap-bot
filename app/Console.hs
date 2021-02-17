module Main where

import App (botConsole, runBot)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (pack, unpack)
import Main.Utf8 (withUtf8)
import System.Console.Haskeline
  ( defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
  )
import System.Console.Pretty
  ( Color (..),
    color,
  )

main :: IO ()
main = withUtf8 $ do
  putStrLn $ color Green "Ldap-bot is listening on terminal"

  runInputT defaultSettings loop
  where
    loop = do
      getInputLine (color Blue ">> ") >>= \case
        Nothing -> return ()
        Just ":q" -> return ()
        Just "" -> loop
        Just input -> do
          message <-
            liftIO $
              runBot (botConsole $ pack input)
                >>= ( \case
                        Left errorMessage -> pure $ color Red errorMessage
                        Right successMessage -> pure $ color White successMessage
                    )
                  . join
          outputStrLn $ unpack message
          loop
