{-# LANGUAGE OverloadedStrings #-}

import Trafikstyrelsen (getVIN)
import Tinglysning (doRequests)

import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import Control.Monad.Trans

import Web.Scotty
import Data.Aeson (ToJSON, encode)
import LandRegisterTypes (LandRegister, Motorregister, Document, Creditor, Debtor, AdditionalText)

-- import Data.Monoid (mconcat)

instance ToJSON LandRegister
instance ToJSON Motorregister
instance ToJSON Document
instance ToJSON Creditor
instance ToJSON Debtor
instance ToJSON AdditionalText


main = scotty 3000 $ do
    get "/registrationnumber/:rn" $ do
        rn <- param "rn"
        result <- liftIO $ search rn
        json $ result

search :: String -> IO TL.Text
search rn = do
  vin <- liftIO $ getVIN (T.pack rn)
  case vin of
    Nothing -> return (TL.pack "Nothing found.")
    Just vin -> do
      landRegister <- doRequests vin
      case landRegister of
        Nothing -> return (TL.pack "Nothing found.")
        Just landRegister -> return $ TLE.decodeUtf8 $ encode landRegister
    
