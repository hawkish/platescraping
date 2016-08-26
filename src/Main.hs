{-# LANGUAGE OverloadedStrings #-}

import Trafikstyrelsen (getSurveyorRapports)
import Tinglysning (getLandRegister)
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
    result <- liftIO $ getSurveyorRapports $ T.pack rn
    json result

  get "/1.0/vin/:vin" $ do
    vin <- param "vin"
    result <- liftIO $ getLandRegister $ T.pack vin
    json result

  notFound $ do
    status status404
    json $ errorJSON "404" "Kan ikke finde servicen."

errorJSON :: String -> String -> TL.Text
errorJSON a b = TLE.decodeUtf8 $ encode $ initError (T.pack a) (T.pack b)

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
