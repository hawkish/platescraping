{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module ErrorType (Error, initError, ProcessingResponseError(..)) where

import qualified Data.Text as T
import GHC.Generics
import Data.Aeson (ToJSON)
import Data.Typeable
import Control.Exception

data Error = MkError { _code :: T.Text
                     , _message :: T.Text
                     } deriving (Eq, Show, Read, Generic)

instance ToJSON Error

initError :: T.Text -> T.Text -> Error
initError a b = MkError { _code = a, _message = b }

data ProcessingResponseError = ParseError String
                             | NoResponseError String
                             deriving (Show, Typeable)

instance Exception ProcessingResponseError
