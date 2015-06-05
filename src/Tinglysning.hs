{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Text.HTML.TagSoup (parseTags, Tag, Tag(..), (~==), (~/=), sections, fromTagText, fromAttrib, isTagText, isTagOpenName, isTagOpen)
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Control.Exception
import Data.List
import Data.List.Split
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy as R
import qualified Data.ByteString.Char8 as B
import Utils (firstMaybe, following, getParameterAt, getCookie)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.List
import Network
import Data.Time.Clock
import Data.Time.Calendar
import qualified Control.Exception as E
import Network.HTTP.Types.Status (statusCode)



n = "<form id=\"j_id4\" name=\"j_id4\" style=\"margin:0px\" method=\"POST\" onkeypress=\"return _submitOnEnter(event,'j_id4');\" action=\"/tinglysning/forespoerg/bilbogen/bilbogen.xhtml;TDK_JSESSIONID=SlEuMoHouvqLGdJ_fICGH8lzaDjd6tNZ0WKFdKJ6vHtRHb7o3ZOj!1927900994!-478567563?_afPfm=-1bav6tyn4e\">\n\n&#9;<div class=\"container-logotop\">\n    <script type=\"text/javascript\" src=\"/tinglysning/js/helptext.js\"></script>\n    <script type=\"text/javascript\" src=\"/tinglysning/js/utils.js\"></script>\n"

h = "/tinglysning/forespoerg/bilbogen/bilbogen.xhtml;TDK_JSESSIONID=E4gvjvzPV_6nECcCdeE8POCn_QtrIGoS0LU6---cO4zQBWaWw0bX!-478567563!1440987210?_afPfm=-11ykkgvvlm&some=234"

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
-- "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"
-- "https://www.tinglysning.dk/tinglysning/common/visdokument/visdokument.xhtml?_afPfm=1avzjdqz5s"

-- Get HTML from trafikstyrelsen.

{--
getParameterAndCookie :: IO (Maybe (String, Cookie))
getParameterAndCookie = do
  response <- getHTMLTinglysning
  case response of
   Nothing -> return Nothing
   Just response -> do
     let parameter = getParameter $ fst response
     case parameter of
      Nothing -> return Nothing
      Just parameter -> do
        let cookie = snd response
        return $ Just (parameter, cookie)
--}

filterAction :: String -> Maybe String
filterAction a = case firstMaybe $ filter (isTagOpenName "form") (parseTags a) of
               Nothing -> Nothing
               Just result -> Just (fromAttrib "action" result)

-- Avoiding case expression ladder with Maybe Monad. 
getParameter :: String -> Maybe String
getParameter a = do
   a1 <- filterAction a
   a2 <- getParameterAt a1 0
   return a2


doFirstRequest :: IO (Maybe (String, Cookie))
doFirstRequest = do
  let url = "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"
  response <- try $ doGetRequest url :: IO (Either SomeException (String, Cookie))
  case response of
   Left ex -> do
     putStrLn $ show ex
     return Nothing
   Right response -> do
     return $ processFirstResponse response 

processFirstResponse :: (String, Cookie) -> Maybe (String, Cookie)
processFirstResponse response = do
  a1 <- filterAction $ fst response
  _afPfm <- getParameterAt a1 0
  let cookie = snd response
  return (_afPfm, cookie)

{--
postFormAtTinglysning :: IO (Maybe (String, Cookie))
postFormAtTinglysning = do
  parameterAndCookie <- getParameterAndCookie
  case parameterAndCookie of
   Nothing -> return Nothing
   Just parameterAndCookie -> do
     let _afPfm = fst parameterAndCookie
     let cookie = snd parameterAndCookie
     let url = "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml∑"
     putStrLn url
     response <- try $ doPostRequest url _afPfm cookie "" :: IO (Either SomeException (String, Cookie))
     case response of
      Left ex -> do
        putStrLn $ show ex
        return Nothing
      Right response -> do
        let html = fst response
        let cookie = snd response
        return $ Just (html, cookie)
--}        
                      

doGetRequest :: String -> IO (String, Cookie)
doGetRequest url = do
  initReq <- parseUrl url
  let req' = initReq { secure = True }
  let req = req' {
        requestHeaders = [("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0")]
        }
  resp <- withManager $ httpLbs req
  let cookieJar = responseCookieJar resp
  let cookie = head $ destroyCookieJar cookieJar
  return (L.unpack $ responseBody resp, cookie)

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


doPostRequest :: String -> String -> Cookie -> String -> IO (String, Cookie)
doPostRequest url _afPfm cookie vin = do
  --let cookie = destroyCookieJar cookieJar
  let name = cookie_name cookie
  let value = cookie_value cookie
  let newcookie = newCookie name value
  let endUrl = url ++ "?" ++ _afPfm
  initReq <- parseUrl endUrl
  let req' = initReq {
        secure = True
        , method = "POST"
        --, cookieJar = Just cookieJar
        , cookieJar = Just $ createCookieJar [newcookie] 
        , requestHeaders = [
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
        }
  let req = urlEncodedBody [("soegemaade", "content:center:bilbogen:stelnrOption"),
                                ("content:center:bilbogen:stelnr", ""),
                                ("content:center:bilbogen:cvr", ""),
                                ("content:center:bilbogen:navn", ""),
                                ("content:center:bilbogen:foedselsdato", ""),
                                ("bogsattest", "content:center:bilbogen:uofficiel"),
                                ("org.apache.myfaces.trinidad.faces.FORM", "j_id4"),
                                ("_noJavaScript", "false"),
                                ("javax.faces.ViewState","!-fkpwmjj67"),
                                ("source","content:center:bilbogen:stelnrOption"),
                                ("event","autosub"),
                                ("partial","true")] $ req'
  
  resp <- withManager $ httpLbs req
  let cookieJar = responseCookieJar resp
  let cookie = head $ destroyCookieJar cookieJar
  return (L.unpack $ responseBody resp, cookie)
 

