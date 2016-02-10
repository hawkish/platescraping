{-# LANGUAGE OverloadedStrings #-}

import Trafikstyrelsen (getSurveyorRapports)
import Tinglysning (getLandRegister)
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import Control.Monad.Trans
import Network.HTTP.Types
import Web.Scotty
import Data.Aeson (ToJSON, encode)
import ErrorType (Error, initError)
import Utils (unescapeJSONText)
--import LandRegisterType (LandRegister, Motorregister, Document, Creditor, Debtor, AdditionalText)
--import SurveyorRapportType (SurveyorRapport, Surveyor, Vehicle, SurveyorDetails, ErrorOverview, ServiceRemarks)

main :: IO ()
main = scotty 3000 $ do

  get "/" $ do
    status status403
    json $ errorJSON "403" "Forbidden."
  
  get "/1.0/registrationnumber/:rn" $ do
    rn <- param "rn"
    result <- liftIO $ searchUsingReg rn
    case result of
      Nothing -> do
        status status404
        json $ errorJSON "404" "Søgningen gav intet resultat."
      Just result -> json result

  get "/1.0/vin/:vin" $ do
    vin <- param "vin"
    if validateVIN vin
      then do 
      result <- liftIO $ searchUsingVin vin
      case result of
        Nothing -> do
          status status404
          json $ errorJSON "404" "Søgningen gav intet resultat."
        Just result -> json result 
      else do
      status status404
      json $ errorJSON "404" "Ikke et gyldigt stel nummer."
     

  notFound $ do
    status status404
    json $ errorJSON "404" "Kan ikke finde servicen."

errorJSON a b = TLE.decodeUtf8 $ encode $ initError (T.pack a) (T.pack b)

searchUsingReg :: String -> IO (Maybe T.Text)
searchUsingReg reg = do
  surveyorRapports <- getSurveyorRapports (T.pack reg)
  if surveyorRapports == []
    then return Nothing
    else return . Just . TL.toStrict . TLE.decodeUtf8 $ encode surveyorRapports
  
searchUsingVin :: String -> IO (Maybe T.Text)
searchUsingVin vin = do
  landRegister <- getLandRegister (T.pack vin)
  case landRegister of
    Nothing -> return Nothing
    Just landRegister -> return . Just . TL.toStrict . TLE.decodeUtf8 $ encode landRegister

-- The rules in this wikipedia article is used.
-- https://en.wikipedia.org/wiki/Vehicle_identification_number
-- No VIN check digit calculation is performed.
-- Skipped the 17 length rule since some VINs are much shorter.
validateVIN :: String -> Bool
validateVIN a = length a > 4 && (and $ map isDigitOrUpperLetter a)

isDigitOrUpperLetter :: Char -> Bool
isDigitOrUpperLetter a
  | isDigit a = True 
  | isLetter a && isUpper a && a /= 'Q' && a /= 'O' && a /= 'I' = True
  | otherwise = False
