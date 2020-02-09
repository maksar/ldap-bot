import           App

import           Data.Text            ( unpack )

import           Control.Monad.Except ( runExceptT )

main :: IO ()
main = runExceptT ldabot >>= either (putStrLn . unpack) return
