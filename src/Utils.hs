{-# LANGUAGE FlexibleContexts #-}
module Utils (firstMaybe, following, getFirstParameter) where

import Data.List.Split
import Data.List
import qualified Text.Parsec as Parsec

-- I am the error message infix operator, used later:
import Text.Parsec ((<?>))

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
import Control.Applicative

-- Get the Identity monad from here:
import Control.Monad.Identity (Identity)

-- alias Parsec.parse for more concise usage.
parse rule text = Parsec.parse rule "(source)" text

h = "/tinglysning/forespoerg/bilbogen/bilbogen.xhtml;TDK_JSESSIONID=E4gvjvzPV_6nECcCdeE8POCn_QtrIGoS0LU6---cO4zQBWaWw0bX!-478567563!1440987210?_afPfm=-11ykkgvvlm&some=234"


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

myParser :: Parsec.Parsec String () String
myParser = do
  letters <- Parsec.many1 Parsec.letter
  return $ letters




