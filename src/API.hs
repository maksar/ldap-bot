{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}

module API (
  type RequiredParam
) where

import           Servant ( QueryParam', Required, Strict )

type RequiredParam = QueryParam' '[Strict, Required]
