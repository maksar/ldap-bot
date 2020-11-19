module API
  ( RequiredParam,
    AccessTokenParam,
  )
where

import Data.Text (Text)
import Servant (QueryParam', Required, Strict)

type RequiredParam = QueryParam' '[Strict, Required]

type AccessTokenParam = RequiredParam "access_token" Text
