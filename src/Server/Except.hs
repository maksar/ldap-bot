module Server.Except where

import           Control.Monad              ( (<=<) )
import           Control.Monad.Trans.Except ( ExceptT, runExceptT )

collapseEitherT :: Monad m => ExceptT c m c -> m c
collapseEitherT = return . either id id <=< runExceptT
