module API (
  RequiredParam,
  AccessTokenParam
) where

import           Servant

import           Data.Text

type RequiredParam = QueryParam' '[Strict, Required]

type AccessTokenParam = RequiredParam "access_token" Text