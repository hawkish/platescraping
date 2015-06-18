{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Utils (getElementAfter, getElementsAfter, getParameterAt, getElementAt, getTagStrings, getTagTexts) where

import Data.List.Split
import Control.Exception
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Text.Parsec as Parsec
import Text.HTML.TagSoup (parseTags, Tag, Tag(..), (~==), (~/=), sections, fromTagText, fromAttrib, isTagText, isTagOpenName, isTagOpen)

-- I am the error message infix operator, used later:
import Text.Parsec ((<?>))

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
import Control.Applicative

-- Get the Identity monad from here:
import Control.Monad.Identity (Identity)

import Control.Applicative
-- alias Parsec.parse for more concise usage.
parse rule text = Parsec.parse rule "(source)" text

h = "/tinglysning/forespoerg/bilbogen/bilbogen.xhtml;TDK_JSESSIONID=E4gvjvzPV_6nECcCdeE8POCn_QtrIGoS0LU6---cO4zQBWaWw0bX!-478567563!1440987210?_afPfm=-11ykkgvvlm&some=234"

g = T.pack "λαθρανασκαφές στην περιοχή της Καρδίτσας"

t = "<h4 class=\"header\">OPLYSNINGER FRA MOTORREGISTER. HVIS INGEN OPLYSNINGER, FINDES K\195\152RET\195\152JET IKKE I DMR:</h4>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">M\195\166rke:</td>\n<td class=\"right\">AUDI A3 2,0 TDI</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">\195\133rgang:</td>\n<td class=\"right\">2009</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Registreringsnummer:</td>\n<td class=\"right\">AM32511</td>\n</tr>\n</tbody>\n</table>\n<div class=\"linefeed\">&#160;</div>\n<hr class=\"long\"/>\n<h2 class=\"subheader\">H\195\134FTELSER</h2>\n<hr class=\"heading\"/>\n<h4 class=\"header\">DOKUMENT:</h4>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Dato/l\195\184benummer:</td>\n<td class=\"right\">13.05.2014-1005340550</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Prioritet:</td>\n<td class=\"right\">1</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Dokument type:</td>\n<td class=\"right\">Ejendomsforbehold</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Hovedstol:</td>\n<td class=\"right\">100.080 DKK</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Rentesats:</td>\n<td class=\"right\">3,95 %</td>\n</tr>\n</tbody>\n</table>\n<div class=\"linefeed\">&#160;</div>\n<div class=\"linefeed\">&#160;</div>\n<hr class=\"heading\"/>\n<h4 class=\"header\">KREDITORER:</h4>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Navn:</td>\n<td class=\"right\">NORDEA FINANS DANMARK A/S</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">CVR:</td>\n<td class=\"right\">89805910</td>\n</tr>\n</tbody>\n</table>\n<div class=\"linefeed\">&#160;</div>\n<hr class=\"heading\"/>\n<h4 class=\"header\">DEBITORER:</h4>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Navn:</td>\n<td class=\"right\">Shahid Hussain Shah</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">CPR:</td>\n<td class=\"right\">010460-****</td>o\n</tr>\n</tbody>\n</table>"

e = T.pack t

getElementAfter :: Eq a => a -> [a] -> Maybe a
getElementAfter a b = do
  index <- elemIndex a b
  result <- getElementAfter' index b 
  return result

getElementAfter' :: Int -> [b] -> Maybe b
getElementAfter' index list = do
  result <- listToMaybe $ drop (index+1) list
  return result

getElementsAfter :: (Eq a) => a -> [a] -> [Maybe a]
getElementsAfter a b = do
  let indices = elemIndices a b
  let result = getElementsAfter' indices b
  getElementsAfter' indices b

getElementsAfter' :: [Int] -> [a] -> [Maybe a]
getElementsAfter' [] list = []
getElementsAfter' (x:xs) list = listToMaybe (drop (x+1) list) : getElementsAfter' xs list

-- Avoiding case expression ladder with Monad. 
getParameterAt :: T.Text -> Int -> Maybe T.Text
getParameterAt a n = do
  a1 <- getParametersAsString a
  a2 <- getParameterAt' a1 n
  return a2

getParametersAsString :: T.Text -> Maybe T.Text
getParametersAsString a = listToMaybe . drop 1 $ T.splitOn "?" a

getParameterAt' :: T.Text -> Int -> Maybe T.Text
getParameterAt' a n = getElementAt list n
                      where list = T.splitOn "&" a
                         
getElementAt :: [a] -> Int -> Maybe a
getElementAt a n = if n > length a - 1
                      then Nothing
                      else Just $ a !! n

getCookie :: String -> Maybe String
getCookie a = do
  a1 <- listToMaybe . drop 1 $ splitOn ";" a
  a2 <- listToMaybe $ splitOn "?" a1
  a3 <- listToMaybe . drop 1 $ splitOn "=" a2
  return a3
                                
myParser :: Parsec.Parsec String () String
myParser = do
  letters <- Parsec.many1 Parsec.letter
  return $ letters

-- Take all relevant tagTexts from the HTML tag soup. Yeah, it's a convoluted process...
getTagStrings :: String -> [String]
getTagStrings = map T.unpack . getTagTexts . T.pack

-- Take all relevant tagTexts from the HTML tag soup. Yeah, it's a convoluted process...
getTagTexts :: T.Text -> [T.Text]
getTagTexts = dequote . map f . filter isTagText . parseTags 
  where f = T.unwords . T.words . fromTagText
        dequote = filter (not . T.null)



