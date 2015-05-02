{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.TagSoup (parseTags, Tag, Tag(..), (~==), (~/=), sections, fromTagText, isTagText)
import Network.HTTP (getResponseBody, getRequest, simpleHTTP, urlEncode)
import Network.HTTP.Conduit
import Control.Exception
import Data.List
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as L

-- Get VIN from trafikstyrelsen.
getVIN :: String -> IO String
getVIN a = do
  let url = "http://selvbetjening.trafikstyrelsen.dk/Sider/resultater.aspx?Reg=" ++ urlEncode a
  result <- try $ getHTML url :: IO (Either SomeException String)
  case result of
   Left ex -> return $ show ex
   Right html -> return $ getVIN' html

-- We're assuming that we're looking for a VIN, so filter the candidates for one.
-- This approach will fail if the tagsoup contains more than one VIN.
getVIN' :: String -> String
getVIN' a = first $ filter isValid (getVINCandidates a)

first :: [String] -> String
first [] = "No VIM found. Or the parser failed."
first a = head a

-- Validator for VIN.
isValid :: String -> Bool
isValid a = length a == 17 && (and $ map isDigitOrUpperLetter a)

isDigitOrUpperLetter :: Char -> Bool
isDigitOrUpperLetter a
  | isDigit a = True 
  | isLetter a && isUpper a = True
  | otherwise = False

-- Refine candidates from the HTML soup. Yeah, it's a convoluted process...
getVINCandidates :: String -> [String]
getVINCandidates a = dequote $ map f $ filter isTagText (parseTags a)
  where f = unwords . words . fromTagText
        dequote = filter (not . null)

getHTML :: String -> IO String
getHTML url = do
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
  

