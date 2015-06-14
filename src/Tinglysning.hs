{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Text.HTML.TagSoup (parseTags, Tag, Tag(..), (~==), (~/=), sections, fromTagText, fromAttrib, isTagText, isTagOpenName, isTagOpen)
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Control.Exception
import Data.List
import Data.List.Split
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B
import Utils (following, getParameterAt, getElementAt, extractFst, extractSnd, extractTrd, extractFrth)
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

doRequests = do
  putStrLn "Doing first request..."
  a1 <- doFstRequest
  case a1 of
   Nothing -> return Nothing
   Just a1 -> do
     let (_afPfm, viewState, cookieList) = a1
     putStrLn _afPfm
     putStrLn viewState
     putStrLn "Doing second request..."
     a2 <- doSndRequest _afPfm viewState cookieList 
     case a2 of
      Nothing -> return Nothing
      Just a2 -> do
        let (_afPfm2, cookieList) = a2
        putStrLn _afPfm2
        -- Maintaining viewState as is.
        putStrLn "Doing third request..."
        a3 <- doTrdRequest "WAUZZZ8P2AA090943" _afPfm2 viewState cookieList
        -- a3 is a HTTP 302 redirect so moving on...
        putStrLn "Doing fourth request..."
        a4 <- doFrthRequest _afPfm2 viewState cookieList
        case a4 of
         Nothing -> return Nothing
         Just a4 -> do
           let (viewState, rangeStart, listItemValue, cookieList) = a4
           putStrLn rangeStart
           putStrLn listItemValue
           putStrLn "Doing fifth request..."
           a5 <- doFfthRequest _afPfm2 rangeStart viewState listItemValue cookieList
           return $ Just a5

{--
doSxthRequest :: String -> String -> [Cookie] -> IO (Maybe (String, String, [Cookie]))
doSxthRequest _afPfm viewState cookie = do
  let url = "https://www.tinglysning.dk/tinglysning/common/visdokument/visdokument.xhtml"
  let requestHeaders = [
          ("Host","www.tinglysning.dk"),
          ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0"),
          ("Accept", "text/html,application/xhtml+xml;q=0.9,*/*;q=0.8"),
          ("Accept-Language", "en-GB,en;q=0.5"),
          ("Accept-Encoding", "gzip, deflate"),
          ("Referer", "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogenresults.xhtml"),
          ("Connection", "keep-alive"),
          ("Pragma", "no-cache"),
          ("Cache-Control", "no-cache")]
  response <- try $ doGetRequest url requestHeaders _afPfm viewState cookie :: IO (Either SomeException (String, [Cookie]))
  case response of
   Left ex -> do
     putStrLn $ show ex
     return Nothing
   Right response -> do
     return $ procFthResponse response
--}
doFfthRequest :: String -> String -> String -> String -> [Cookie] -> IO (Maybe (String, [Cookie]))
doFfthRequest _afPfm rangeStart viewState listItemValue cookie = do
  let url = "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogenresults.xhtml"
  let referer = url ++ "?" ++ _afPfm
  putStrLn viewState
  putStrLn referer
  let requestHeaders = [
          ("Host","www.tinglysning.dk"),
          ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0"),
          ("Accept", "text/html,application/xhtml+xml;application/xml;q=0.9,*/*;q=0.8"),
          ("Accept-Language", "en-GB,en;q=0.5"),
          ("Accept-Encoding", "gzip, deflate"),
          ("Referer", B.pack(referer)),
          ("Connection", "keep-alive")]
  let body = [
        ("content:center:bilbogenresults:bilerid:rangeStart", B.pack(rangeStart)),
        ("org.apache.myfaces.trinidad.faces.FORM", "j_id4"),
        ("_noJavaScript", "false"),
        ("javax.faces.ViewState", B.pack(viewState)),
        ("source","content:center:bilbogenresults:bilerid:0:visbildetaljer"),
        ("state", ""),
        ("value", ""),
        ("listItem", B.pack(listItemValue))]
  response <- try $ doPostRequest url requestHeaders body _afPfm cookie :: IO (Either SomeException (String, [Cookie]))
  case response of
   Left ex -> do
     putStrLn $ show ex
     return Nothing
   Right response -> do
     return $ Just response

doFrthRequest :: String -> String -> [Cookie] -> IO (Maybe (String, String, String, [Cookie]))
doFrthRequest _afPfm viewState cookie = do
  let url = "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogenresults.xhtml"
  let requestHeaders = [
          ("Host","www.tinglysning.dk"),
          ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0"),
          ("Accept", "text/html,application/xhtml+xml;q=0.9,*/*;q=0.8"),
          ("Accept-Language", "en-GB,en;q=0.5"),
          ("Accept-Encoding", "gzip, deflate"),
          ("Referer", "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"),
          ("Connection", "keep-alive"),
          ("Pragma", "no-cache"),
          ("Cache-Control", "no-cache")]
  response <- try $ doGetRequest url requestHeaders _afPfm viewState cookie :: IO (Either SomeException (String, [Cookie]))
  case response of
   Left ex -> do
     putStrLn $ show ex
     return Nothing
   Right response -> do
     return $ procFrthResponse response

procFrthResponse ::  (String, [Cookie]) -> Maybe (String, String, String, [Cookie])
procFrthResponse response = do
  let html = fst response
  let cookieList = snd response
  viewState <- filterInputViewState html
  rangeStart <- filterInputRangeStart html
  listItemValue <- getListItemValue html
  return (viewState, rangeStart, listItemValue, cookieList)

doTrdRequest :: String -> String -> String -> [Cookie] -> IO (Maybe (String, [Cookie]))
doTrdRequest vin _afPfm viewState cookie = do
  let url = "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"
  let requestHeaders = [
          ("Host","www.tinglysning.dk"),
          ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0"),
          ("Accept", "text/html,application/xhtml+xml;q=0.9,*/*;q=0.8"),
          ("Accept-Language", "en-GB,en;q=0.5"),
          ("Accept-Encoding", "gzip, deflate"),
          ("Referer", B.pack(url)),
          ("Connection", "keep-alive"),
          ("Pragma", "no-cache"),
          ("Cache-Control", "no-cache")]
  let body = [
        ("soegemaade", "content:center:bilbogen:stelnrOption"),
        ("content:center:bilbogen:stelnr", B.pack(vin)),
        ("content:center:bilbogen:cvr", ""),
        ("content:center:bilbogen:navn", ""),
        ("content:center:bilbogen:foedselsdato", ""),
        ("bogsattest", "content:center:bilbogen:uofficiel"),
        ("org.apache.myfaces.trinidad.faces.FORM", "j_id4"),
        ("_noJavaScript", "false"),
        ("javax.faces.ViewState", B.pack(viewState)),
        ("source","content:center:bilbogen:j_id150")]
  response <- try $ doPostRequest url requestHeaders body _afPfm cookie :: IO (Either SomeException (String, [Cookie]))
  case response of
   Left ex -> do
     putStrLn $ show ex
     return Nothing
   Right response -> do
     return $ Just response

doSndRequest :: String -> String -> [Cookie] -> IO (Maybe (String, [Cookie]))
doSndRequest _afPfm viewState cookie = do
  let url = "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"
  let requestHeaders = [
          ("Host","www.tinglysning.dk"),
          ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0"),
          ("Accept", "text/html,application/xhtml+xml;q=0.9,*/*;q=0.8"),
          ("Accept-Language", "en-GB,en;q=0.5"),
          ("Accept-Encoding", "gzip, deflate"),
          ("Referer", B.pack(url)),
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
        ("javax.faces.ViewState", B.pack(viewState)),
        ("source","content:center:bilbogen:stelnrOption"),
        ("event","autosub"),
        ("partial","true")]
  response <- try $ doPostRequest url requestHeaders body _afPfm cookie :: IO (Either SomeException (String, [Cookie]))
  case response of
   Left ex -> do
     putStrLn $ show ex
     return Nothing
   Right response -> do
     return $ procSndResponse response

procSndResponse :: (String, [Cookie]) -> Maybe (String, [Cookie])
procSndResponse response = do
  a1 <- filterContent $ fst response
  _afPfm <- getParameterAt a1 0
  let cookieList = snd response
  return (_afPfm, cookieList)


doFstRequest :: IO (Maybe (String, String, [Cookie]))
doFstRequest = do
  let url = "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"
  let requestHeaders = [
          ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0")]
  response <- try $ doSimplerGetRequest url requestHeaders :: IO (Either SomeException (String, [Cookie]))
  case response of
   Left ex -> do
     putStrLn $ show ex
     return Nothing
   Right response -> do
     return $ procFstResponse response 

procFstResponse :: (String, [Cookie]) -> Maybe (String, String, [Cookie])
procFstResponse response = do
  a1 <- filterForm $ fst response
  _afPfm <- getParameterAt a1 0
  let cookieList = snd response
  viewState <- filterInputViewState $ fst response
  return (_afPfm, viewState, cookieList)

filterForm :: String -> Maybe String
filterForm a = case listToMaybe $ filter (isTagOpenName "form") (parseTags a) of
               Nothing -> Nothing
               Just result -> Just (fromAttrib "action" result)

filterInputViewState :: String -> Maybe String
filterInputViewState a = case listToMaybe $ filter (~== ("<input name=javax.faces.ViewState" :: String)) $ filter (isTagOpenName "input") (parseTags a) of
                 Nothing -> Nothing
                 Just result -> Just (fromAttrib "value" result)

filterContent :: String -> Maybe String
filterContent a = case listToMaybe $ filter (isTagOpenName "content") (parseTags a) of
               Nothing -> Nothing
               Just result -> Just (fromAttrib "action" result)

filterAnchor :: String -> Maybe String
filterAnchor a = case listToMaybe $ filter (~== ("<a name=content:center:bilbogenresults:bilerid:0:visbildetaljer" :: String)) $ filter (isTagOpenName "a") (parseTags a) of
                 Nothing -> Nothing
                 Just result -> Just (fromAttrib "onclick" result)


getListItemValue :: String -> Maybe String
getListItemValue a = do
  a1 <- filterAnchor a
  let elem = splitOn "'" a1
  indexListItem <- elemIndex "listItem" elem
  let indexListItemValue = indexListItem + 2
  a2 <- getElementAt elem indexListItemValue
  return a2

filterInputRangeStart :: String -> Maybe String
filterInputRangeStart a = case listToMaybe $ filter (~== ("<input name=content:center:bilbogenresults:bilerid:rangeStart" :: String)) $ filter (isTagOpenName "input") (parseTags a) of
                 Nothing -> Nothing
                 Just result -> Just (fromAttrib "value" result)

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

doSimplerGetRequest :: String -> RequestHeaders -> IO (String, [Cookie])
doSimplerGetRequest url requestHeadersList = do
  putStrLn url
  initReq <- parseUrl url
  let req = initReq {
        secure = True
        , method = "GET"
        , requestHeaders = requestHeadersList
        }
  resp <- withManager $ httpLbs req
  let cookieJar = responseCookieJar resp
  let cookieList = destroyCookieJar cookieJar
  return (LB.unpack $ responseBody resp, cookieList)

doGetRequest :: String -> RequestHeaders -> String -> String -> [Cookie] -> IO (String, [Cookie])
doGetRequest baseUrl requestHeadersList _afPfm viewState cookie = do
  let url = baseUrl ++ "?" ++ _afPfm
  putStrLn url
  initReq <- parseUrl url
  let req = initReq {
        secure = True
        , method = "GET"
        , cookieJar = Just $ createCookieJar cookie
        , requestHeaders = requestHeadersList
        }
  resp <- withManager $ httpLbs req
  let cookieJar = responseCookieJar resp
  let cookieList = destroyCookieJar cookieJar
  return (LB.unpack $ responseBody resp, cookieList)

doPostRequest :: String -> RequestHeaders -> [(B.ByteString, B.ByteString)] -> String -> [Cookie] -> IO (String, [Cookie])
doPostRequest baseUrl requestHeadersList body _afPfm cookie = do
  let url = baseUrl ++ "?" ++ _afPfm
  putStrLn url
  initReq <- parseUrl url
  let req' = initReq {
        secure = True
        , method = "POST"
        , cookieJar = Just $ createCookieJar cookie
        , requestHeaders = requestHeadersList
        , redirectCount = 0
        }
  let req = urlEncodedBody body $ req'
  resp <- withManager $ httpLbs req
  
  let cookieJar = responseCookieJar resp
  let cookieList = destroyCookieJar cookieJar
  return (LB.unpack $ responseBody resp, cookieList)
  
