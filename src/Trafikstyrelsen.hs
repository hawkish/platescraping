{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Trafikstyrelsen (getVIN) where

import Text.HTML.TagSoup (parseTags, Tag, Tag(..), (~==), (~/=), sections, fromTagText, fromAttrib, isTagText, isTagOpenName, isTagOpen)
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Control.Exception
import Data.List
import Data.List.Split
import Data.Char
import Data.Maybe
import Control.Monad.Trans
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import Utils (getElementAfter, getTagTexts)


getVIN :: T.Text -> IO (Maybe T.Text)
getVIN a = do
  a1 <- liftIO $ getHTMLTrafikStyrelsen a
  case a1 of
    Nothing -> return Nothing
    Just a1 -> return $ getVIN' a1
      
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


