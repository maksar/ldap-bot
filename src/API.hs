module API (
  RequiredParam
) where

import           Servant

type RequiredParam = QueryParam' '[Strict, Required]
