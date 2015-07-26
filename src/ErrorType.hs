{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module ErrorType (Error, initError) where

import qualified Data.Text    as T
import Control.Lens
import GHC.Generics

data Error = MkError { _code :: T.Text
                     , _message :: T.Text
                     } deriving (Eq, Show, Read, Generic)

makeLenses ''Error

initError a b = MkError { _code = a, _message = b }
