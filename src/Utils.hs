{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Utils (getElementAfter, getElementsAfter, getParameterAt, getElementAt, getTagStrings, getTagTexts, getOpenTags, dequote, extractText, getTextAfter, getTextsAfter, getTextAfterAt, deleteEveryNth) where

import Data.List.Split
import Data.List (elemIndex, elemIndices) 
import Data.Maybe

import qualified Data.Text as T
import Text.HTML.TagSoup (parseTags, fromTagText, isTagText, isTagOpen, fromAttrib, Tag)


getTextAfterAt :: T.Text -> Int -> T.Text -> Maybe T.Text
getTextAfterAt a c b = getElementAfter a c $ getTagTexts b

getTextAfter :: T.Text -> T.Text -> Maybe T.Text
getTextAfter a b = getElementAfter a 1 $ dequote $ getTagTexts b

getTextsAfter :: T.Text -> T.Text -> [Maybe T.Text]
getTextsAfter a b = getElementsAfter a $ dequote $ getTagTexts b

getOpenTags :: T.Text -> [Tag T.Text]
getOpenTags = filter isTagOpen . parseTags

dequote :: [T.Text] -> [T.Text]
dequote = filter (not . T.null) 

getElementAfter :: Eq a => a -> Int -> [a] -> Maybe a
getElementAfter a c b = do
  index <- elemIndex a b
  result <- getElementAfter' index c b 
  return result

getElementAfter' :: Int -> Int -> [b] -> Maybe b
getElementAfter' index count list = do
  listToMaybe $ drop (index+count) list

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
getParametersAsString = listToMaybe . drop 1 . T.splitOn "?" 

getParameterAt' :: T.Text -> Int -> Maybe T.Text
getParameterAt' a n = getElementAt (T.splitOn "&" a) n
                         
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
getTagStrings = map T.unpack . dequote . getTagTexts . T.pack

-- Take all relevant tagTexts from the HTML tag soup. Yeah, it's a convoluted process...
getTagTexts :: T.Text -> [T.Text]
getTagTexts = map extractText . filter isTagText . parseTags 

deleteEveryNth :: Int -> [a] -> [a]    
deleteEveryNth n = concat . map init . group n

group :: Int -> [a] -> [[a]]
group n [] = []
group n xs = take n xs : group n (drop n xs)

extractText :: Tag T.Text -> T.Text
extractText = removeBreaks . fromTagText

removeBreaks :: T.Text -> T.Text
removeBreaks = T.unwords . T.words


