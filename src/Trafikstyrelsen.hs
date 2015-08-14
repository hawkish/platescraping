{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Trafikstyrelsen (getVIN) where


import Network.HTTP.Client
import Control.Exception
import Data.Char
import Control.Monad.Trans
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import Utils (getElementAfter, getTagTexts)


getVIN :: T.Text -> IO (Maybe T.Text)
getVIN a = do
  a1 <- liftIO $ getHTMLTrafikStyrelsen a
  case a1 of
    Nothing -> return Nothing
    Just a1 -> return $ getVIN' a1

getLinks :: T.Text -> [T.Text]
getLinks = filterLink . splitAtQuote . getLocationHref . getOnClick . getOpenTags

filterLink :: [T.Text] -> [T.Text]
filterLink a = filter (T.isInfixOf "/Sider") a 

splitAtQuote :: [T.Text] -> [T.Text]
splitAtQuote = concat . map (T.split (== '\"'))

getLocationHref :: [T.Text] -> [T.Text]
getLocationHref a = filter (T.isInfixOf "location.href") a 

getOnClick :: [Tag T.Text] -> [T.Text]
getOnClick = dequote . map (fromAttrib "onclick")

-- We're assuming, that we're looking for a valid VIN after "Stelnummer".
getVIN' a = do
  a1 <- parseVIN a
  a2 <- validateVIN a1
  return a2


parseVIN :: T.Text -> Maybe T.Text
parseVIN = getElementAfter "Stelnummer" . getTagTexts

-- The rules in this wikipedia article is used.
-- https://en.wikipedia.org/wiki/Vehicle_identification_number
-- No VIN check digit calculation is performed.
validateVIN :: T.Text -> Maybe T.Text
validateVIN a
  | isValid a = Just a
  | otherwise = Nothing

isValid :: T.Text -> Bool
isValid = isValid' . T.unpack 

isValid' :: String -> Bool
isValid' a = length a == 17 && (and $ map isDigitOrUpperLetter a)

isDigitOrUpperLetter :: Char -> Bool
isDigitOrUpperLetter a
  | isDigit a = True 
  | isLetter a && isUpper a && a /= 'Q' && a /= 'O' && a /= 'I' = True
  | otherwise = False

getHTMLTrafikStyrelsen :: T.Text -> IO (Maybe T.Text)
getHTMLTrafikStyrelsen a = do
  manager <- liftIO $ newManager defaultManagerSettings
  let baseUrl = T.pack "http://selvbetjening.trafikstyrelsen.dk/Sider/resultater.aspx?Reg="
  let url = baseUrl `T.append` a
  html <- try $ doGetRequest manager url :: IO (Either SomeException T.Text)
  case html of
   Left ex -> do
     putStrLn $ show ex
     return Nothing
   Right html -> return $ Just html

doGetRequest :: Manager -> T.Text -> IO T.Text
doGetRequest manager url = do
  initReq <- liftIO $ parseUrl $ T.unpack url
  let req = initReq {
        method = "GET"
        }
  resp <- httpLbs req manager
  return (TL.toStrict $ TLE.decodeUtf8 $ responseBody resp)


