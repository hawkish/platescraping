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
import Utils (getElementAfter, getTagTexts, getOpenTags, dequote)
import Text.HTML.TagSoup (parseTags, fromTagText, isTagText, isTagOpen, fromAttrib, Tag)
import SurveyorRapportType (initSurveyorRapport, SurveyorRapport)

t = T.pack "<h2>Fejloversigt<span class=\"floatRightNoClear infoText hideOnSmall\">Hold musen over tallet for beskrivelse</span><span class=\"floatRightNoClear infoText hideOnLarge hideOnMedium\">Tryk p\229 tallet for beskrivelse</span></h2></div>\r\n\t<div class=\"clear\"></div>\r\n    <div class=\"errorList\">\r\n        \r\n        \r\n                <div class=\"number\" title='Bremser'>5</div>\r\n\t            <div class=\"information\">st\230nksk\230rm, t\230ret, venstre, bag</div>\r\n            \r\n                <div class=\"number\" title='El-anl\230g, lygter, reflekser mv.'>6</div>\r\n\t            <div class=\"information\">nummerpladelygte, virker ikke, venstre</div>\r\n            \r\n    </div>\r\n</div>\r\n\r\n<div class=\"clear\"></div>"

a = T.pack "/Sider/synsrapport.aspx?Inspection=12319436&Vin=SJNFAAN16U0057657"

h = T.pack "This is a no result string."

--getVIN :: T.Text -> IO (Maybe T.Text)
getVIN a = do
  a1 <- liftIO $ getVINHTML a
  case a1 of
    Nothing -> return Nothing
    Just a1 -> return $ Just $ getLinks a1


getSurveyorRapport a = do
  a1 <- liftIO $ getSurveyorHTML a
  case a1 of
   Nothing -> return Nothing
   Just a1 -> return $ Just $ initSurveyorRapport a1
   --Just a1 -> return $ Just $ getTagTexts a1

getLinks a = do
  a1 <- getLinks' a
  return a1

getLinks' :: T.Text -> [T.Text]
getLinks' = filterLink . splitAtQuote . getLocationHref . getOnClick . getOpenTags

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
parseVIN = getElementAfter "Stelnummer" 0 . dequote . getTagTexts

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

getVINHTML :: T.Text -> IO (Maybe T.Text)
getVINHTML a = do
  manager <- liftIO $ newManager defaultManagerSettings
  let baseUrl = T.pack "http://selvbetjening.trafikstyrelsen.dk/Sider/resultater.aspx?Reg="
  let url = baseUrl `T.append` a
  html <- try $ doGetRequest manager url :: IO (Either SomeException T.Text)
  case html of
   Left ex -> do
     putStrLn $ show ex
     return Nothing
   Right html -> return $ Just html

getSurveyorHTML :: T.Text -> IO (Maybe T.Text)
getSurveyorHTML a = do
  manager <- liftIO $ newManager defaultManagerSettings
  let baseUrl = T.pack "http://selvbetjening.trafikstyrelsen.dk"
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


