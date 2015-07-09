{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Tinglysning (doRequests) where

import Text.HTML.TagSoup (parseTags, Tag, Tag(..), (~==), (~/=), sections, fromTagText, maybeTagText, fromAttrib, isTagText, isTagOpenName, isTagOpen)
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Control.Exception
import Data.List
import Data.List.Split
import Data.Conduit
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Lazy as LBL
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import Utils (getElementAfter, getElementsAfter, getParameterAt, getElementAt, getTagTexts)
import LandRegisterTypes (initCreditor, initDebtor, initMotorregister, initDocument, initAdditionalText, initLandRegister, Creditor, Debtor, Motorregister, Document, AdditionalText, LandRegister)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.List
import Network
import Data.Time.Clock
import Data.Time.Calendar
import Data.Maybe
import qualified Control.Exception as E
import Network.HTTP.Types.Status (statusCode)

n = "<form id=\"j_id4\" name=\"j_id4\" style=\"margin:0px\" method=\"POST\" onkeypress=\"return _submitOnEnter(event,'j_id4');\" action=\"/tinglysning/forespoerg/bilbogen/bilbogen.xhtml;TDK_JSESSIONID=SlEuMoHouvqLGdJ_fICGH8lzaDjd6tNZ0WKFdKJ6vHtRHb7o3ZOj!1927900994!-478567563?_afPfm=-1bav6tyn4e\">\n\n&#9;<div class=\"container-logotop\">\n    <script type=\"text/javascript\" src=\"/tinglysning/js/helptext.js\"></script>\n    <script type=\"text/javascript\" src=\"/tinglysning/js/utils.js\"></script>\n"

h = "/tinglysning/forespoerg/bilbogen/bilbogen.xhtml;TDK_JSESSIONID=E4gvjvzPV_6nECcCdeE8POCn_QtrIGoS0LU6---cO4zQBWaWw0bX!-478567563!1440987210?_afPfm=-11ykkgvvlm&some=234"

url = "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"

a = "<input type=\"hidden\" name=\"_noJavaScript\" value=\"false\"><span id=\"tr_j_id4_Postscript\"><input type=\"hidden\" name=\"javax.faces.ViewState\" value=\"!-tcw82nrpf\"><input type=\"hidden\" name=\"source\">"

b = "<?xml version=\"1.0\" ?>\n<?Tr-XHR-Response-Type ?>\n<content action=\"/tinglysning/forespoerg/bilbogen/bilbogen.xhtml?_afPfm=-x8n0v5c5u\"> \n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n<fragment>"

c = "<td class=\"af_column_cell-text OraTableBorder1111\"><span id=\"content:center:bilbogenresults:bilerid:0:stelnummer\">WAUZZZ8P2AA090943</span></td><td class=\"af_column_cell-text OraTableBorder1111\">AM32511</td><td class=\"af_column_cell-text OraTableBorder1111\">AUDI</td><td class=\"af_column_cell-text OraTableBorder1111\">2009</td><td class=\"af_column_cell-text OraTableBorder1111\"></td><td class=\"af_column_cell-text OraTableBorder1111\"><a id=\"content:center:bilbogenresults:bilerid:0:visbildetaljer\" name=\"content:center:bilbogenresults:bilerid:0:visbildetaljer\" onclick=\"submitForm('j_id4',1,{source:'content:center:bilbogenresults:bilerid:0:visbildetaljer','listItem':'f2922aa1-de72-4be6-8dc2-c57610a7c4ad'});return false;\" class=\"OraLink\" href=\"#\">Vis</a></td></tr></table></td></tr></table><script type=\"text/javascript\">_uixt_content_center_bilbogenresults_bilerid=new CollectionComponent('j_id4','content:center:bilbogenresults:bilerid');</script><input type=\"hidden\" name=\"content:center:bilbogenresults:bilerid:rangeStart\" value=\"0\"></div></div>\n\n  <p></p><input id=\"content:center:bilbogenresults:j_id118\" name=\"content:center:bilbogenresults:j_id118\" type=\"submit\" value=\"(S)&oslash;g igen\" onclick=\"submitForm('j_id4',1,{source:'content:center:bilbogenresults:j_id118'});return false;\" accesskey=\"S\">\n\n<br>\n<br>\n</td>"

d = "<h4 class=\"header\">OPLYSNINGER FRA MOTORREGISTER. HVIS INGEN OPLYSNINGER, FINDES K\195\152RET\195\152JET IKKE I DMR:</h4>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">M\195\166rke:</td>\n<td class=\"right\">AUDI A3 2,0 TDI</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">\195\133rgang:</td>\n<td class=\"right\">2009</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Registreringsnummer:</td>\n<td class=\"right\">AM32511</td>\n</tr>\n</tbody>\n</table>\n<div class=\"linefeed\">&#160;</div>\n<hr class=\"long\"/>\n<h2 class=\"subheader\">H\195\134FTELSER</h2>\n<hr class=\"heading\"/>\n<h4 class=\"header\">DOKUMENT:</h4>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Dato/l\195\184benummer:</td>\n<td class=\"right\">13.05.2014-1005340550</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Prioritet:</td>\n<td class=\"right\">1</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Dokument type:</td>\n<td class=\"right\">Ejendomsforbehold</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Hovedstol:</td>\n<td class=\"right\">100.080 DKK</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Rentesats:</td>\n<td class=\"right\">3,95 %</td>\n</tr>\n</tbody>\n</table>\n<div class=\"linefeed\">&#160;</div>\n<div class=\"linefeed\">&#160;</div>\n<hr class=\"heading\"/>\n<h4 class=\"header\">KREDITORER:</h4>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Navn:</td>\n<td class=\"right\">NORDEA FINANS DANMARK A/S</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">CVR:</td>\n<td class=\"right\">89805910</td>\n</tr>\n</tbody>\n</table>\n<div class=\"linefeed\">&#160;</div>\n<hr class=\"heading\"/>\n<h4 class=\"header\">DEBITORER:</h4>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Navn:</td>\n<td class=\"right\">Shahid Hussain Shah</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">CPR:</td>\n<td class=\"right\">010460-****</td>\n</tr>\n</tbody>\n</table>"

doRequests :: IO (Maybe LandRegister)
doRequests = do
  let vin = "WAUZZZ8P2AA090943"
  manager <- liftIO $ newManager conduitManagerSettings 
  putStrLn "Doing first request..."
  a1 <- doFstRequest manager
  case a1 of
   Nothing -> do
     closeManager manager
     return Nothing
   Just a1 -> do
     let (_afPfm, viewState, cookieList) = a1
     putStrLn "Doing second request..."
     a2 <- doSndRequest manager _afPfm viewState cookieList 1 
     case a2 of
      Nothing -> do
        closeManager manager
        return Nothing
      Just a2 -> do
        let (_afPfm2, cookieList) = a2
        -- Maintaining viewState as is.
        putStrLn "Doing third request..."
        -- a3 is a redirect. 
        a3 <- doTrdRequest manager vin _afPfm2 viewState cookieList 1
        case a3 of
         Nothing -> do
           closeManager manager
           return Nothing
         Just a3 -> do
           let (viewState, rangeStart, listItemValue, cookieList) = a3
           putStrLn "Doing fourth request..."
           a4 <- doFrthRequest manager _afPfm2 rangeStart viewState listItemValue cookieList 1
           -- a4 is also a redirect. 
           case a4 of
            Nothing -> do
              closeManager manager
              return Nothing
            Just a4 -> do
              closeManager manager
              let (html, cookieList) = a4
              return $ Just $ initLandRegister html

doFrthRequest :: Manager -> T.Text -> T.Text -> T.Text -> T.Text -> [Cookie] -> Int -> IO (Maybe (T.Text, [Cookie]))
doFrthRequest manager _afPfm rangeStart viewState listItemValue cookie redirects = do
  let url = T.pack "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogenresults.xhtml"
  let referer = url `T.append` (T.pack "?") `T.append` _afPfm
  let requestHeaders = [
          ("Host","www.tinglysning.dk"),
          ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0"),
          ("Accept", "text/html,application/xhtml+xml;application/xml;q=0.9,*/*;q=0.8"),
          ("Accept-Language", "en-GB,en;q=0.5"),
          ("Accept-Encoding", "gzip, deflate"),
          ("Referer", TE.encodeUtf8(referer)),
          ("Connection", "keep-alive")]
  let body = [
        ("content:center:bilbogenresults:bilerid:rangeStart", TE.encodeUtf8(rangeStart)),
        ("org.apache.myfaces.trinidad.faces.FORM", "j_id4"),
        ("_noJavaScript", "false"),
        ("javax.faces.ViewState", TE.encodeUtf8(viewState)),
        ("source","content:center:bilbogenresults:bilerid:0:visbildetaljer"),
        ("state", ""),
        ("value", ""),
        ("listItem", TE.encodeUtf8(listItemValue))]
  response <- try $ doPostRequest manager url requestHeaders body _afPfm cookie redirects :: IO (Either SomeException (T.Text, [Cookie]))
  case response of
   Left ex -> do
     putStrLn $ show ex
     return Nothing
   Right response -> do
     return $ Just response

doTrdRequest :: Manager -> T.Text -> T.Text -> T.Text -> [Cookie] -> Int -> IO (Maybe (T.Text, T.Text, T.Text, [Cookie]))
doTrdRequest manager vin _afPfm viewState cookie redirects = do
  let url = T.pack "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"
  let requestHeaders = [
          ("Host","www.tinglysning.dk"),
          ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0"),
          ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"),
          ("Accept-Language", "en-GB,en;q=0.5"),
          ("Accept-Encoding", "gzip, deflate"),
          ("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8"),
          ("Referer", TE.encodeUtf8(url)),
          ("Connection", "keep-alive"),
          ("Pragma", "no-cache"),
          ("Cache-Control", "no-cache")]
  let body = [
        ("soegemaade", "content:center:bilbogen:stelnrOption"),
        ("content:center:bilbogen:stelnr", TE.encodeUtf8(vin)),
        ("content:center:bilbogen:cvr", ""),
        ("content:center:bilbogen:navn", ""),
        ("content:center:bilbogen:foedselsdato", ""),
        ("bogsattest", "content:center:bilbogen:uofficiel"),
        ("org.apache.myfaces.trinidad.faces.FORM", "j_id4"),
        ("_noJavaScript", "false"),
        ("javax.faces.ViewState", TE.encodeUtf8(viewState)),
        ("source","content:center:bilbogen:j_id150")]
  response <- try $ doPostRequest manager url requestHeaders body _afPfm cookie redirects :: IO (Either SomeException (T.Text, [Cookie]))
  putStrLn "Done third request."
  case response of
   Left ex -> do
     putStrLn $ show ex
     return Nothing
   Right response -> do
     return $ procTrdResponse response

procTrdResponse ::  (T.Text, [Cookie]) -> Maybe (T.Text, T.Text, T.Text, [Cookie])
procTrdResponse response = do
  let html = fst response
  let cookieList = snd response
  viewState <- filterInputViewState html
  rangeStart <- filterInputRangeStart html
  listItemValue <- getListItemValue html
  return (viewState, rangeStart, listItemValue, cookieList)


doSndRequest :: Manager -> T.Text -> T.Text -> [Cookie] -> Int -> IO (Maybe (T.Text, [Cookie]))
doSndRequest manager _afPfm viewState cookie redirects = do
  let url = T.pack "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"
  let requestHeaders = [
          ("Host","www.tinglysning.dk"),
          ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0"),
          ("Accept", "text/html,application/xhtml+xml;application/xml;q=0.9,*/*;q=0.8"),
          ("Accept-Language", "en-GB,en;q=0.5"),
          ("Accept-Encoding", "gzip, deflate"),
          ("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8"),
          ("Referer", TE.encodeUtf8(url)),
          ("Tr-XHR-Message","true"),
          ("Connection", "keep-alive"),
          ("Pragma", "no-cache"),
          ("Cache-Control", "no-cache")]
  let body = [
        ("soegemaade", "content:center:bilbogen:stelnrOption"),
        ("content:center:bilbogen:stelnr", ""),
        ("content:center:bilbogen:cvr", ""),
        ("content:center:bilbogen:navn", ""),
        ("content:center:bilbogen:foedselsdato", ""),
        ("bogsattest", "content:center:bilbogen:uofficiel"),
        ("org.apache.myfaces.trinidad.faces.FORM", "j_id4"),
        ("_noJavaScript", "false"),
        ("javax.faces.ViewState", TE.encodeUtf8(viewState)),
        ("source","content:center:bilbogen:stelnrOption"),
        ("event","autosub"),
        ("partial","true")]
  response <- try $ doPostRequest manager url requestHeaders body _afPfm cookie redirects :: IO (Either SomeException (T.Text, [Cookie]))
  putStrLn "Done second request."
  case response of
   Left ex -> do
     putStrLn $ show ex
     return Nothing
   Right response -> do
     return $ procSndResponse response

procSndResponse :: (T.Text, [Cookie]) -> Maybe (T.Text, [Cookie])
procSndResponse response = do
  a1 <- filterXMLContent $ fst response
  _afPfm <- getParameterAt a1 0
  let cookieList = snd response
  return (_afPfm, cookieList)


doFstRequest :: Manager -> IO (Maybe (T.Text, T.Text, [Cookie]))
doFstRequest manager = do
  let url = T.pack "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"
  let requestHeaders = [
          ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0"),
           ("Accept", "text/html,application/xhtml+xml;application/xml;q=0.9,*/*;q=0.8"),
           ("Accept-Language", "en-GB,en;q=0.5"),
           ("Accept-Encoding", "gzip, deflate"),
           ("Connection", "keep-alive")]
  response <- try $ doSimplerGetRequest manager url requestHeaders :: IO (Either SomeException (T.Text, [Cookie]))
  putStrLn "Done first request."
  case response of
   Left ex -> do
     putStrLn $ show ex
     return Nothing
   Right response -> do
     return $ procFstResponse response 

procFstResponse :: (T.Text, [Cookie]) -> Maybe (T.Text, T.Text, [Cookie])
procFstResponse response = do
  a1 <- filterForm $ fst response
  _afPfm <- getParameterAt a1 0
  let cookieList = snd response
  viewState <- filterInputViewState $ fst response
  return (_afPfm, viewState, cookieList)

filterForm :: T.Text -> Maybe T.Text
filterForm a = case listToMaybe $ filter (isTagOpenName "form") (parseTags a) of
               Nothing -> Nothing
               Just result -> Just (fromAttrib "action" result)

filterInputViewState :: T.Text -> Maybe T.Text
filterInputViewState a = case listToMaybe $ filter (~== ("<input name=javax.faces.ViewState" :: String)) $ filter (isTagOpenName "input") (parseTags a) of
                 Nothing -> Nothing
                 Just result -> Just (fromAttrib "value" result)

filterXMLContent :: T.Text -> Maybe T.Text
filterXMLContent a = case listToMaybe $ filter (isTagOpenName "content") (parseTags a) of
               Nothing -> Nothing
               Just result -> Just (fromAttrib "action" result)

filterAnchor :: T.Text -> Maybe T.Text
filterAnchor a = case listToMaybe $ filter (~== ("<a name=content:center:bilbogenresults:bilerid:0:visbildetaljer" :: String)) $ filter (isTagOpenName "a") (parseTags a) of
                 Nothing -> Nothing
                 Just result -> Just (fromAttrib "onclick" result)

filterInputRangeStart :: T.Text -> Maybe T.Text
filterInputRangeStart a = case listToMaybe $ filter (~== ("<input name=content:center:bilbogenresults:bilerid:rangeStart" :: String)) $ filter (isTagOpenName "input") (parseTags a) of
                 Nothing -> Nothing
                 Just result -> Just (fromAttrib "value" result)

getListItemValue :: T.Text -> Maybe T.Text
getListItemValue a = do
  a1 <- filterAnchor a
  let elem = T.splitOn "'" a1
  indexListItem <- elemIndex "listItem" elem
  let indexListItemValue = indexListItem + 2
  a2 <- getElementAt elem indexListItemValue
  return a2


{--
past :: UTCTime
past = UTCTime (ModifiedJulianDay 56200) (secondsToDiffTime 0)

future :: UTCTime
future = UTCTime (ModifiedJulianDay 562000) (secondsToDiffTime 0)

newCookie :: B.ByteString -> B.ByteString -> Cookie
newCookie name value = Cookie { cookie_name = name
                 , cookie_value = value
                 , cookie_expiry_time = future
                 , cookie_domain = "www.tinglysning.dk"
                 , cookie_path = "/"
                 , cookie_creation_time = past
                 , cookie_last_access_time = past
                 , cookie_persistent = False
                 , cookie_host_only = True
                 , cookie_secure_only = False
                 , cookie_http_only = True
                 }

--}

doSimplerGetRequest :: Manager -> T.Text -> RequestHeaders -> IO (T.Text, [Cookie])
doSimplerGetRequest manager url requestHeadersList = do
  initReq <- liftIO $ parseUrl $ T.unpack url
  let req = initReq {
        secure = True
        , method = "GET"
        , requestHeaders = requestHeadersList
        }
  resp <- httpLbs req manager
  let cookieJar = responseCookieJar resp
  let cookieList = destroyCookieJar cookieJar
  return (TL.toStrict $ TLE.decodeUtf8 $ responseBody resp, cookieList)


doGetRequest :: Manager -> T.Text -> RequestHeaders -> T.Text -> T.Text -> [Cookie] -> IO (T.Text, [Cookie])
doGetRequest manager baseUrl requestHeadersList _afPfm viewState cookie = do
  let url = baseUrl `T.append` (T.pack "?") `T.append` _afPfm
  initReq <- liftIO $ parseUrl $ T.unpack url
  let req = initReq {
        secure = True
        , method = "GET"
        , cookieJar = Just $ createCookieJar cookie
        , requestHeaders = requestHeadersList
        }
  resp <- httpLbs req manager
  let cookieJar = responseCookieJar resp
  let cookieList = destroyCookieJar cookieJar
  return (TL.toStrict $ TLE.decodeUtf8 $ responseBody resp, cookieList)


doPostRequest :: Manager -> T.Text -> RequestHeaders -> [(B.ByteString, B.ByteString)] -> T.Text -> [Cookie] -> Int -> IO (T.Text, [Cookie])
doPostRequest manager baseUrl requestHeadersList body _afPfm cookie redirects = do
  let url = baseUrl `T.append` (T.pack "?") `T.append` _afPfm
  initReq <- liftIO $ parseUrl $ T.unpack url
  let req' = initReq {
        secure = True
        , method = "POST"
        , cookieJar = Just $ createCookieJar cookie
        , requestHeaders = requestHeadersList
        , redirectCount = redirects
        }
  let req = urlEncodedBody body $ req'
  resp <- httpLbs req manager 
  let cookieJar = responseCookieJar resp
  let cookieList = destroyCookieJar cookieJar
  return (TL.toStrict $ TLE.decodeUtf8 $ responseBody resp, cookieList)
  
