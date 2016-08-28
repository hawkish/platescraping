{-# LANGUAGE OverloadedStrings #-}

import Trafikstyrelsen (getSurveyorRapports)
import Tinglysning (getLandRegister)
import ErrorType
import LandRegisterType
import SurveyorRapportType
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import Control.Monad.Trans
import Network.HTTP.Types
import Web.Scotty
import Data.Aeson (encode)
import ErrorType (initError)
import Control.Exception

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    status status403
    json $ errorJSON "403" "Forbidden."
  
  get "/1.0/registrationnumber/:rn" $ do
    rn <- param "rn"
    result <- liftIO $ searchUsingReg rn
    case result of
      Left ex -> do
        status status404
        json $ errorJSON "404" $ show ex
      Right val -> json val

  get "/1.0/vin/:vin" $ do
    vin <- param "vin"
    result <- liftIO $ searchUsingVin vin
    case result of
      Left ex -> do
        status status404
        json $ errorJSON "404" $ show ex
      Right val -> json val

  notFound $ do
    status status404
    json $ errorJSON "404" "Kan ikke finde servicen."

errorJSON :: String -> String -> Error
errorJSON a b = initError (T.pack a) (T.pack b)

searchUsingReg :: String -> IO (Either SomeException [SurveyorRapport])
searchUsingReg reg = do
  surveyorRapports <- try $ getSurveyorRapports (T.pack reg)
  return surveyorRapports

searchUsingVin :: String -> IO (Either SomeException (Maybe LandRegister))
searchUsingVin vin = do
  landRegister <- try $ getLandRegister (T.pack vin)
  return landRegister

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
