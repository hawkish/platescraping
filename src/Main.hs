{-# LANGUAGE OverloadedStrings #-}

import Trafikstyrelsen (getSurveyorRapports)
import Tinglysning (getLandRegister)
import ErrorType (initError, Error)
import Data.String (fromString)
import Text.HTML.TagSoup (parseTags, fromTagText, isTagText)

import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import Control.Monad.Trans
import Network.HTTP.Types
import Web.Scotty
import Data.Aeson (ToJSON, encode)
import LandRegisterType (LandRegister, Motorregister, Document, Creditor, Debtor, AdditionalText)

instance ToJSON LandRegister
instance ToJSON Motorregister
instance ToJSON Document
instance ToJSON Creditor
instance ToJSON Debtor
instance ToJSON AdditionalText
instance ToJSON Error


main :: IO ()
main = scotty 3000 $ do

  get "/" $ do
    status status403
    let error = TLE.decodeUtf8 $ encode $ initError (T.pack "403") (T.pack $ fromString $ "Forbidden.")
    json error
  
  get "/1.0/registrationnumber/:rn" $ do
    rn <- param "rn"
    result <- liftIO $ searchUsingReg rn
    case result of
      Nothing -> do
        status status404
        let error = TLE.decodeUtf8 $ encode $ initError (T.pack "404") (T.pack "Søgningen gav ikke noget resultat.")
        json error
      Just result -> json result

  get "/1.0/vin/:vin" $ do
    vin <- param "vin"
    result <- liftIO $ searchUsingVin vin
    case result of
      Nothing -> do
        status status404
        let error = TLE.decodeUtf8 $ encode $ initError (T.pack "404") (T.pack "Søgningen gav ikke noget resultat.")
        json error
      Just result -> json result

  notFound $ do
    status status404
    let error = TLE.decodeUtf8 $ encode $ initError (T.pack "404") (T.pack $ fromString $ "Kan ikke finde servicen.")
    json error

searchUsingReg :: String -> IO (Maybe TL.Text)
searchUsingReg reg = do
  surveyorRapports <- getSurveyorRapports (T.pack reg)
  case surveyorRapports of
    Nothing -> return Nothing
    Just surveyorRapports -> return $ Just $ TLE.decodeUtf8 $ encode surveyorRapports

searchUsingVin :: String -> IO (Maybe TL.Text)
searchUsingVin vin = do
  landRegister <- getLandRegister (T.pack vin)
  case landRegister of
    Nothing -> return Nothing
    Just landRegister -> return $ Just $ TLE.decodeUtf8 $ encode landRegister
