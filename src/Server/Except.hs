module Server.Except where

import           Control.Monad              ( (<=<) )
import           Control.Monad.Trans.Except ( ExceptT, runExceptT )

collapseExceptT :: Monad m => ExceptT c m c -> m c
collapseExceptT = return . either id id <=< runExceptT
