{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Text.HTML.TagSoup (parseTags, Tag, Tag(..), (~==), (~/=), sections, fromTagText, fromAttrib, isTagText, isTagOpenName, isTagOpen)
import Network.HTTP.Conduit
import Control.Exception
import Data.List
import Data.List.Split
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as L
import Utils (firstMaybe, following, getParameters)



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

getTinglysning :: IO (String)
getTinglysning = do
  result <- getHTMLTinglysning
  case result of
   Left ex -> return $ show ex
   Right html -> case getAction html of
                  Nothing -> return "Nothing found. Or the parser failed."
                  Just result -> return result

getAction :: String -> Maybe String
getAction a = case getForm a of
               Nothing -> Nothing
               Just result -> Just (fromAttrib "action" result)

getForm :: String -> Maybe (Tag String)
getForm a = firstMaybe $ filter (isTagOpenName "form") (parseTags a)

--isPfmPresent :: Text -> Text -> Bool
isPfmPresent a = isInfixOf "_afPfm=" a

getHTMLTinglysning :: IO (Either SomeException String)
getHTMLTinglysning = do
  let url = "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"
  result <- try $ doGetRequest_C url :: IO (Either SomeException String)
  return result


doGetRequest_C :: String -> IO String
doGetRequest_C url = do
  req0 <- parseUrl url
  let req = req0 {
        requestHeaders = [("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0")]
        }
  resp <- withManager $ httpLbs req
  return $ L.unpack $ responseBody resp
  

