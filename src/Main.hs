
import Text.HTML.TagSoup (parseTags, Tag, Tag(..), (~==), (~/=), sections, fromTagText)
import Network.HTTP (getResponseBody, getRequest, simpleHTTP, urlEncode)
import Control.Exception

main = putStrLn "hello"

t = "<div class=\"horizontalSplitter\"></div>\r\n\r\n        <div class=\"floatLeft grid5\">\r\n            <h2>K\195\184ret\195\184j</h2>\r\n            \r\n            <div id=\"ctl00_m_g_4156985a_4cd3_409b_aab5_4416025b40bb_ctl00_pnlVehicleInfo\">\r\n\t\t\t\t\t\t\r\n            <div class=\"pairName\">M\195\166rke</div>\r\n            <div class=\"pairValue\">AUDI</div>\r\n            <div class=\"pairName\">Model</div>\r\n            <div class=\"pairValue\">A3</div>\r\n            <div class=\"pairName\">Stelnummer</div>\r\n            <div class=\"pairValue\">WAUZZZ8P2AA090943</div>\r\n            <div class=\"pairName\">Seneste reg.nr.</div>\r\n            <div class=\"pairValue\">AM32511</div>\r\n            \r\n            \r\n\t\t\t\t\t</div>\r\n            <div class=\"clear\"></div><br /><br />\r\n        </div>\r\n        <div class=\"floatRight grid7\">\r\n"

findDivs :: String -> [Tag String]
findDivs a = filter (~== TagOpen "div" [("class","pairValue")]) (parseTags a)

--fDivs :: String -> [Tag String]
fDivs a = sections (~== "<div class=\"pairValue\">") (parseTags a)

getText a = fromTagText (dropWhile (~/= "<div class=\"pairValue\">") a !! 1)

getRegNumber :: String -> IO String
getRegNumber a = do
  let url = "http://selvbetjening.trafikstyrelsen.dk/Sider/resultater.aspx?Reg=" ++ urlEncode a
  result <- try $ getHTML url :: IO (Either SomeException String)
  case result of
   Left ex -> return $ show ex
   Right html -> return html


getHTML :: String -> IO String
getHTML url = do
  resp <- simpleHTTP $ getRequest $ url
  html <- getResponseBody resp
  return html
    



