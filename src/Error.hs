{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE TypeOperators    #-}

module Error (
  runErrorCollapsing,
  collapse2,
  (<?>)
) where

import           Control.Monad.Freer       ( Eff, Member )
import           Control.Monad.Freer.Error ( Error, runError, throwError )

import           Data.Text                 ( Text )

collapse2 :: (a -> b -> Eff (Error e : effs) e) -> a -> b -> Eff effs e
collapse2 program a b = runErrorCollapsing $ program a b

runErrorCollapsing :: Eff (Error e : effs) e -> Eff effs e
runErrorCollapsing x = do
  result <- runError x
  return $ either id id result

(<?>) :: Member (Error Text) effs => Either e a -> Text -> Eff effs a
result <?> errorMessage = case result of
  Left _      -> throwError errorMessage
  Right value -> return value

