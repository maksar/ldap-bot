module Main where

import App (botConsole, runBot)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (isPrefixOf, tails)
import Data.Text (pack, unpack)
import Main.Utf8 (withUtf8)
import System.Console.Haskeline
  ( Settings,
    completeWord,
    defaultSettings,
    getInputLine,
    historyFile,
    outputStrLn,
    runInputT,
    setComplete,
    simpleCompletion,
  )
import System.Console.Pretty
  ( Color (..),
    color,
  )
import System.Directory (getHomeDirectory)

settings :: MonadIO m => FilePath -> Settings m
settings home =
  setComplete
    ( completeWord
        Nothing
        []
        ( \start ->
            pure (map simpleCompletion $ filter (any (start `isPrefixOf`) . tails) ["/list", "/add", "/remove"])
        )
    )
    $ defaultSettings {historyFile = Just (home <> "/.ldap-bot-console")}

main :: IO ()
main = withUtf8 $ do
  putStrLn $ color Green "Ldap-bot is listening on terminal"
  home <- getHomeDirectory
  runInputT (settings home) loop
  where
    loop = do
      getInputLine (color Blue ">> ") >>= \case
        Nothing -> return ()
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
