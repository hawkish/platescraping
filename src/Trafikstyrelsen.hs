{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Trafikstyrelsen (getSurveyorRapports) where

import Network.HTTP.Client
import Control.Exception
-- import Data.Char
-- import Control.Monad
import Control.Monad.Trans
-- import Control.Monad.Trans.Maybe
--import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import Utils (getOpenTags, dequote)
import Text.HTML.TagSoup (fromAttrib, Tag)
import SurveyorRapportType (initSurveyorRapport, SurveyorRapport)

getSurveyorRapports :: T.Text -> IO [Maybe SurveyorRapport]
getSurveyorRapports a = do
  -- Getting link(s) to the surveyor rapport(s).
  a1 <- getSurveyorLinks a
  case a1 of
    Nothing -> return []
    Just a2 -> do
      -- Using mapM :: (a -> mb) -> [a] -> m[b]
      -- In this case: (a -> IO(b)) -> [a] -> IO[b]
      a3 <- mapM getSurveyorRapport a2
      -- Using return to wrap into IO
      return a3

getSurveyorLinks :: T.Text -> IO (Maybe [T.Text])
getSurveyorLinks a = do
  -- Unwrap the IO result for a1.
  a1 <- getVINHTML a
  -- Using fmap to unwrap Maybe for parseLinks
  -- Using return to wrap into IO
  return $ fmap parseLinks a1

getSurveyorRapport :: T.Text -> IO (Maybe SurveyorRapport)
getSurveyorRapport a = do
  a1 <- getSurveyorHTML a
  return $ fmap initSurveyorRapport a1

parseLinks :: T.Text -> [T.Text]
parseLinks = filterLink . splitAtQuote . filterLocationHref . getOnClick . getOpenTags

filterLink :: [T.Text] -> [T.Text]
filterLink a = filter (T.isInfixOf "/Sider") a 

splitAtQuote :: [T.Text] -> [T.Text]
splitAtQuote = concat . map (T.split (== '\"'))

filterLocationHref :: [T.Text] -> [T.Text]
filterLocationHref a = filter (T.isInfixOf "location.href") a 

getOnClick :: [Tag T.Text] -> [T.Text]
getOnClick = dequote . map (fromAttrib "onclick")

getVINHTML :: T.Text -> IO (Maybe T.Text)
getVINHTML a = do
  manager <- liftIO $ newManager defaultManagerSettings
  let baseUrl = T.pack "http://selvbetjening.trafikstyrelsen.dk/Sider/resultater.aspx?Reg="
  let url = baseUrl `T.append` a
  html <- try $ doGetRequest manager url :: IO (Either SomeException T.Text)
  case html of
   Left ex -> do
     putStrLn $ show ex
     return Nothing
   Right html -> return $ Just html

getSurveyorHTML :: T.Text -> IO (Maybe T.Text)
getSurveyorHTML a = do
  manager <- liftIO $ newManager defaultManagerSettings
  let baseUrl = T.pack "http://selvbetjening.trafikstyrelsen.dk"
  let url = baseUrl `T.append` a
  html <- try $ doGetRequest manager url :: IO (Either SomeException T.Text)
  case html of
   Left ex -> do
     putStrLn $ show ex
     return Nothing
   Right html -> return $ Just html


doGetRequest :: Manager -> T.Text -> IO T.Text
doGetRequest manager url = do
  initReq <- liftIO $ parseUrl $ T.unpack url
  let req = initReq {
        method = "GET"
        }
  resp <- httpLbs req manager
  return (TL.toStrict $ TLE.decodeUtf8 $ responseBody resp)


