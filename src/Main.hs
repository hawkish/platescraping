
import Text.HTML.TagSoup (parseTags, Tag, Tag(..), (~==), (~/=), sections, fromTagText, isTagText)
import Network.HTTP (getResponseBody, getRequest, simpleHTTP, urlEncode)
import Control.Exception
import Data.List
import Data.Char

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
first [] = "No VIM found by parser."
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
    



