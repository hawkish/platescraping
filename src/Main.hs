{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Text.HTML.TagSoup (parseTags, Tag, Tag(..), (~==), (~/=), sections, fromTagText, fromAttrib, isTagText, isTagOpenName, isTagOpen)
import Network.HTTP (getResponseBody, getRequest, simpleHTTP, urlEncode)
import Network.HTTP.Conduit
import Control.Exception
import Data.List 
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as L
--import qualified Data.ByteString.Char8



t = "<div class=\"horizontalSplitter\"></div>\r\n\r\n        <div class=\"floatLeft grid5\">\r\n            <h2>K\195\184ret\195\184j</h2>\r\n            \r\n            <div id=\"ctl00_m_g_4156985a_4cd3_409b_aab5_4416025b40bb_ctl00_pnlVehicleInfo\">\r\n\t\t\t\t\t\t\r\n            <div class=\"pairName\">M\195\166rke</div>\r\n            <div class=\"pairValue\">AUDI</div>\r\n            <div class=\"pairName\">Model</div>\r\n            <div class=\"pairValue\">A3</div>\r\n            <div class=\"pairName\">Stelnummer</div>\r\n            <div class=\"pairValue\">WAUZZZ8P2AA090943</div>\r\n            <div class=\"pairName\">Seneste reg.nr.</div>\r\n            <div class=\"pairValue\">AM32511</div>\r\n            \r\n            \r\n\t\t\t\t\t</div>\r\n            <div class=\"clear\"></div><br /><br />\r\n        </div>\r\n        <div class=\"floatRight grid7\">\r\n"

n = "<form id=\"j_id4\" name=\"j_id4\" style=\"margin:0px\" method=\"POST\" onkeypress=\"return _submitOnEnter(event,'j_id4');\" action=\"/tinglysning/forespoerg/bilbogen/bilbogen.xhtml;TDK_JSESSIONID=SlEuMoHouvqLGdJ_fICGH8lzaDjd6tNZ0WKFdKJ6vHtRHb7o3ZOj!1927900994!-478567563?_afPfm=-1bav6tyn4e\">\n\n&#9;<div class=\"container-logotop\">\n    <script type=\"text/javascript\" src=\"/tinglysning/js/helptext.js\"></script>\n    <script type=\"text/javascript\" src=\"/tinglysning/js/utils.js\"></script>\n"

-- Get HTML from trafikstyrelsen.
getVIN :: String -> IO String
getVIN a = do
  result <- getHTMLTrafikStyrelsen a
  case result of
   Left ex -> return $ show ex
   Right html -> case parseHTMLTrafikstyrelsen html of
                  Nothing -> return "Nothing found. Or the parser failed."
                  Just result -> return result

-- We're assuming, that we're looking for a VIN, so filter the tagtexts for valid ones.
-- Also we're only including all tagtexts *after* "Stelnummer" as candidates.
-- In fact we're hoping that the relevant VIN is present immediately *after* "Stelnummer"
-- - but not counting on it. And any other VINs are excluded by first.
parseHTMLTrafikstyrelsen :: String -> Maybe String
parseHTMLTrafikstyrelsen a = firstMaybe $ filter isValid candidates
          where candidates = following "Stelnummer" $ getTagTexts a

firstMaybe :: [a] -> Maybe a
firstMaybe [] = Nothing
firstMaybe a = Just $ head a

following :: Eq a => a -> [a] -> [a]
following a b =
  case elemIndex a b of
   Nothing -> b
   Just index -> if result == [] then b else result
                                         where result = drop (index+1) b


-- Take all relevant tagTexts from the HTML soup. Yeah, it's a convoluted process...
getTagTexts :: String -> [String]
getTagTexts a = dequote $ map f $ filter isTagText (parseTags a)
  where f = unwords . words . fromTagText
        dequote = filter (not . null)

-- Validator for Vehicle Identification Number (VIN).
-- http://en.wikipedia.org/wiki/Vehicle_identification_number
isValid :: String -> Bool
isValid a = length a == 17 && (and $ map isDigitOrUpperLetter a)

isDigitOrUpperLetter :: Char -> Bool
isDigitOrUpperLetter a
  | isDigit a = True 
  | isLetter a && isUpper a && a /= 'Q' && a /= 'O' && a /= 'I' = True
  | otherwise = False

getHTMLTrafikStyrelsen :: String -> IO (Either SomeException String)
getHTMLTrafikStyrelsen a = do
  let url = "http://selvbetjening.trafikstyrelsen.dk/Sider/resultater.aspx?Reg=" ++ urlEncode a
  result <- try $ doGetRequest url :: IO (Either SomeException String)
  return result

doGetRequest :: String -> IO String
doGetRequest url = do
  resp <- simpleHTTP $ getRequest $ url
  html <- getResponseBody resp
  return html



--soegemaade:content:center:bilbogen:stelnrOption
--content:center:bilbogen:stelnr:WAUZZZ8P2AA090943
--content:center:bilbogen:cvr:
--content:center:bilbogen:navn:
--content:center:bilbogen:foedselsdato:
--bogsattest:content:center:bilbogen:uofficiel
--org.apache.myfaces.trinidad.faces.FORM:j_id4
--_noJavaScript:false
--javax.faces.ViewState:!-pwa1crud3
--source:content:center:bilbogen:j_id150
-- "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"
-- "https://www.tinglysning.dk/tinglysning/common/visdokument/visdokument.xhtml?_afPfm=1avzjdqz5s"

-- Get HTML from trafikstyrelsen.

getTinglysning :: IO (String)
getTinglysning = do
  result <- getHTMLTinglysning
  case result of
   Left ex -> return $ show ex
   Right html -> case getAction html of
                  Nothing -> return "Nothing found. Or the parser failed."
                  Just result -> return result

getAction :: String -> Maybe String
getAction a = case getForm a of
               Nothing -> Nothing
               Just result -> Just (fromAttrib "action" result)

getForm :: String -> Maybe (Tag String)
getForm a = firstMaybe $ filter (isTagOpenName "form") (parseTags a)

--isPfmPresent :: Text -> Text -> Bool
isPfmPresent a = isInfixOf "_afPfm=" a

getHTMLTinglysning :: IO (Either SomeException String)
getHTMLTinglysning = do
  let url = "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"
  result <- try $ doGetRequest_C url :: IO (Either SomeException String)
  return result


doGetRequest_C :: String -> IO String
doGetRequest_C url = do
  req0 <- parseUrl url
  let req = req0 {
        requestHeaders = [("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0")]
        }
  resp <- withManager $ httpLbs req
  return $ L.unpack $ responseBody resp
  

