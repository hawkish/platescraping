module Utils (firstMaybe, following, getFirstParameter) where

import Data.List.Split
import Data.List

firstMaybe :: [a] -> Maybe a
firstMaybe [] = Nothing
firstMaybe a = Just $ head a

following :: Eq a => a -> [a] -> [a]
following a b =
  case elemIndex a b of
   Nothing -> b
   Just index -> if result == [] then b else result
                                         where result = drop (index+1) b

getParametersAsString :: String -> Maybe String
getParametersAsString a = firstMaybe . drop 1 $ splitOn "?" a

getFirstParameter' :: String -> Maybe String
getFirstParameter' a = firstMaybe $ splitOn "&" a

-- Avoiding case expression ladder with Maybe Monad. 
getFirstParameter :: String -> Maybe String
getFirstParameter a = do
  a1 <- getParametersAsString a
  a2 <- getFirstParameter' a1
  return a2



