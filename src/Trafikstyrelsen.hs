{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Text.HTML.TagSoup (parseTags, Tag, Tag(..), (~==), (~/=), sections, fromTagText, fromAttrib, isTagText, isTagOpenName, isTagOpen)
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Control.Exception
import Data.List
import Data.List.Split
import Data.Char
import Data.Maybe
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text.Encoding as TE
import Utils (getElementAfter, getTagTexts)


t = "<div class=\"horizontalSplitter\"></div>\r\n\r\n        <div class=\"floatLeft grid5\">\r\n            <h2>K\195\184ret\195\184j</h2>\r\n            \r\n            <div id=\"ctl00_m_g_4156985a_4cd3_409b_aab5_4416025b40bb_ctl00_pnlVehicleInfo\">\r\n\t\t\t\t\t\t\r\n            <div class=\"pairName\">M\195\166rke</div>\r\n            <div class=\"pairValue\">AUDI</div>\r\n            <div class=\"pairName\">Model</div>\r\n            <div class=\"pairValue\">A3</div>\r\n            <div class=\"pairName\">Stelnummer</div>\r\n            <div class=\"pairValue\">WAUZZZ8P2AA090943</div>\r\n            <div class=\"pairName\">Seneste reg.nr.</div>\r\n            <div class=\"pairValue\">AM32511</div>\r\n            \r\n            \r\n\t\t\t\t\t</div>\r\n            <div class=\"clear\"></div><br /><br />\r\n        </div>\r\n        <div class=\"floatRight grid7\">\r\n"

-- Get HTML from trafikstyrelsen.
--getVIN :: T.Text -> IO (Maybe T.Text)
--getVIN a = do
--  result <- getHTMLTrafikStyrelsen a
--  case result of
--   Nothing -> return Nothing
--   Just html -> return $ parseHTMLTrafikstyrelsen html


-- We're assuming, that we're looking for a VIN, so filter the tagtexts for valid ones.
-- Also we're only including all tagtexts *after* "Stelnummer" as candidates.
-- In fact we're hoping that the relevant VIN is present immediately *after* "Stelnummer"
-- - but not counting on it. And any other VINs are excluded by first.
{--
parseHTMLTrafikstyrelsen :: T.Text -> Maybe T.Text
parseHTMLTrafikstyrelsen = listToMaybe . T.pack . filter isValid . T.unpack . getElementAfter "Stelnummer" . getTagTexts

-- Validator for Vehicle Identification Number (VIN).
-- http://en.wikipedia.org/wiki/Vehicle_identification_number
isValid :: String -> Bool
isValid a = T.length a == 17 && (and $ map isDigitOrUpperLetter a)

isDigitOrUpperLetter :: Char -> Bool
isDigitOrUpperLetter a
  | isDigit a = True 
  | isLetter a && isUpper a && a /= 'Q' && a /= 'O' && a /= 'I' = True
  | otherwise = False
--}
getHTMLTrafikStyrelsen :: T.Text -> IO (Maybe T.Text)
getHTMLTrafikStyrelsen a = do
  let baseUrl = T.pack "http://selvbetjening.trafikstyrelsen.dk/Sider/resultater.aspx?Reg="
  let url = baseUrl `T.append` a
  html <- try $ doGetRequest url :: IO (Either SomeException T.Text)
  case html of
   Left ex -> do
     putStrLn $ show ex
     return Nothing
   Right html -> return $ Just html

doGetRequest :: T.Text -> IO T.Text
doGetRequest url = do
  initReq <- parseUrl $ T.unpack url
  let req = initReq {
        method = "GET"
        }
  resp <- withManager $ httpLbs req
  return $ T.pack $ LB.unpack $ responseBody resp

--doGetRequest :: String -> IO String
--doGetRequest url = do
  --resp <- simpleHTTP $ getRequest url
  --html <- getResponseBody resp
  --return html

