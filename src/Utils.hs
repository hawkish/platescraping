{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Utils (getElementAfter, getElementsAfter, getParameterAt, getElementAt, getTagStrings, getTagTexts, getOpenTags, dequote, getTextAfter, getTagTexts) where

import Data.List.Split
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Text.HTML.TagSoup (parseTags, fromTagText, isTagText, isTagOpen, fromAttrib, Tag)

t = "<h2>Synssted</h2><div class=\"pairName\">Virksomhed</div>\r\n    <div class=\"pairValue\">A-Inspektion - Kolding</div>\r\n\t<div class=\"pairName\">CVR</div>\r\n\t<div class=\"pairValue\">28496818</div>\r\n\t<div class=\"pairName\">Sted</div>\r\n\t<div class=\"pairValue\">Vonsildvej 23<br/>6000 Kolding</div>\r\n\t<div class=\"clear\"></div>"

h = T.pack t

h1 = T.pack "This is a no result string."

getTextAfter :: T.Text -> T.Text -> Maybe T.Text
getTextAfter a b = getElementAfter a $ getTagTexts b

getTextsAfter :: T.Text -> T.Text -> [Maybe T.Text]
getTextsAfter a b = getElementsAfter a $ getTagTexts b

getOpenTags :: T.Text -> [Tag T.Text]
getOpenTags a = filter isTagOpen (parseTags a)

dequote :: [T.Text] -> [T.Text]
dequote = filter (not . T.null) 

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
                                
-- Take all relevant tagTexts from the HTML tag soup. Yeah, it's a convoluted process...
getTagStrings :: String -> [String]
getTagStrings = map T.unpack . getTagTexts . T.pack

-- Take all relevant tagTexts from the HTML tag soup. Yeah, it's a convoluted process...
getTagTexts :: T.Text -> [T.Text]
getTagTexts = dequote . map f . filter isTagText . parseTags 
  where f = T.unwords . T.words . fromTagText

