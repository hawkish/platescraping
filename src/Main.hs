{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.TagSoup (parseTags, Tag, Tag(..), (~==), (~/=), sections, fromTagText, isTagText)
import Network.HTTP (getResponseBody, getRequest, simpleHTTP, urlEncode)
import Network.HTTP.Conduit
import Control.Exception
import Data.List
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as L


-- Get HTML from trafikstyrelsen.
getVIN :: String -> IO String
getVIN a = do
  result <- getHTML a
  case result of
   Left ex -> return $ show ex
   Right html -> return $ parse $ html

first :: [String] -> String
first [] = "No VIM found. Or the parser failed."
first a = head a

following :: Eq a => a -> [a] -> [a]
following a b =
  case elemIndex a b of
    Just i -> drop i b
    Nothing -> b

-- We're assuming, that we're looking for a VIN, so filter the tagtexts for valid ones.
-- Also we're only including all tagtexts *after* "Stelnummer" as candidates.
-- In fact we're hoping that the relevant VIN is present immediately *after* "Stelnummer"
-- - but not counting on it. And any other VINs are excluded by first.
parse :: String -> String
parse a = first $ filter isValid candidates
          where candidates = following "Stelnummer" $ getTagTexts a

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

getHTML :: String -> IO (Either SomeException String)
getHTML a = do
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

getHTMLTLS = do
  req0 <- parseUrl "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"
  let req = req0 {
        --method = methodPost
        requestHeaders = [("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0")]
                                  --, requestBody = RequestBodyLBS "{\"longUrl\": \"http://www.google.com/\"}"
        }

  res <- withManager $ httpLbs req
  L.putStrLn $ responseBody res
  

