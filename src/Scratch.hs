{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
module Scratch () where

import Text.HTML.TagSoup (parseTags, Tag, Tag(..), (~==), (~/=), sections, fromTagText, fromAttrib, isTagText, isTagOpenName, isTagOpen)
import Utils (getElementAfter, getElementsAfter, getElementAt, getTagTexts)
import Data.Maybe
import Data.List.Split
import Data.List
import qualified Data.ByteString.Char8 as B
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import Control.Lens


g = "λαθρανασκαφές στην περιοχή της Καρδίτσας"


n = T.pack "<form id=\"j_id4\" name=\"j_id4\" style=\"margin:0px\" method=\"POST\" onkeypress=\"return _submitOnEnter(event,'j_id4');\" action=\"/tinglysning/forespoerg/bilbogen/bilbogen.xhtml;TDK_JSESSIONID=SlEuMoHouvqLGdJ_fICGH8lzaDjd6tNZ0WKFdKJ6vHtRHb7o3ZOj!1927900994!-478567563?_afPfm=-1bav6tyn4e\">\n\n&#9;<div class=\"container-logotop\">\n    <script type=\"text/javascript\" src=\"/tinglysning/js/helptext.js\"></script>\n    <script type=\"text/javascript\" src=\"/tinglysning/js/utils.js\"></script>\n"


a = T.pack "<input type=\"hidden\" name=\"_noJavaScript\" value=\"false\"><span id=\"tr_j_id4_Postscript\"><input type=\"hidden\" name=\"javax.faces.ViewState\" value=\"!-tcw82nrpf\"><input type=\"hidden\" name=\"source\">"


b = T.pack "<?xml version=\"1.0\" ?>\n<?Tr-XHR-Response-Type ?>\n<content action=\"/tinglysning/forespoerg/bilbogen/bilbogen.xhtml?_afPfm=-x8n0v5c5u\"> \n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n<fragment>"

c = T.pack "<td class=\"af_column_cell-text OraTableBorder1111\"><span id=\"content:center:bilbogenresults:bilerid:0:stelnummer\">WAUZZZ8P2AA090943</span></td><td class=\"af_column_cell-text OraTableBorder1111\">AM32511</td><td class=\"af_column_cell-text OraTableBorder1111\">AUDI</td><td class=\"af_column_cell-text OraTableBorder1111\">2009</td><td class=\"af_column_cell-text OraTableBorder1111\"></td><td class=\"af_column_cell-text OraTableBorder1111\"><a id=\"content:center:bilbogenresults:bilerid:0:visbildetaljer\" name=\"content:center:bilbogenresults:bilerid:0:visbildetaljer\" onclick=\"submitForm('j_id4',1,{source:'content:center:bilbogenresults:bilerid:0:visbildetaljer','listItem':'f2922aa1-de72-4be6-8dc2-c57610a7c4ad'});return false;\" class=\"OraLink\" href=\"#\">Vis</a></td></tr></table></td></tr></table><script type=\"text/javascript\">_uixt_content_center_bilbogenresults_bilerid=new CollectionComponent('j_id4','content:center:bilbogenresults:bilerid');</script><input type=\"hidden\" name=\"content:center:bilbogenresults:bilerid:rangeStart\" value=\"0\"></div></div>\n\n  <p></p><input id=\"content:center:bilbogenresults:j_id118\" name=\"content:center:bilbogenresults:j_id118\" type=\"submit\" value=\"(S)&oslash;g igen\" onclick=\"submitForm('j_id4',1,{source:'content:center:bilbogenresults:j_id118'});return false;\" accesskey=\"S\">\n\n<br>\n<br>\n</td>"

e = T.pack "<h4 class=\"header\">OPLYSNINGER FRA MOTORREGISTER. HVIS INGEN OPLYSNINGER, FINDES K\195\152RET\195\152JET IKKE I DMR:</h4>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Mærke:</td>\n<td class=\"right\">AUDI A3 2,0 TDI</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Årgang:</td>\n<td class=\"right\">2009</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Registreringsnummer:</td>\n<td class=\"right\">AM32511</td>\n</tr>\n</tbody>\n</table>\n<div class=\"linefeed\">&#160;</div>\n<hr class=\"long\"/>\n<h2 class=\"subheader\">H\195\134FTELSER</h2>\n<hr class=\"heading\"/>\n<h4 class=\"header\">DOKUMENT:</h4>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Dato/løbenummer:</td>\n<td class=\"right\">13.05.2014-1005340550</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Prioritet:</td>\n<td class=\"right\">1</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Dokument type:</td>\n<td class=\"right\">Ejendomsforbehold</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Hovedstol:</td>\n<td class=\"right\">100.080 DKK</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Rentesats:</td>\n<td class=\"right\">3,95 %</td>\n</tr>\n</tbody>\n</table>\n<div class=\"linefeed\">&#160;</div>\n<div class=\"linefeed\">&#160;</div>\n<hr class=\"heading\"/>\n<h4 class=\"header\">KREDITORER:</h4>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Navn:</td>\n<td class=\"right\">NORDEA FINANS DANMARK A/S</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">CVR:</td>\n<td class=\"right\">89805910</td>\n</tr>\n</tbody>\n</table>\n<div class=\"linefeed\">&#160;</div>\n<hr class=\"heading\"/>\n<h4 class=\"header\">DEBITORER:</h4>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Navn:</td>\n<td class=\"right\">Shahid Hussain Shah</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">CPR:</td>\n<td class=\"right\">010460-****</td>o\n</tr>\n</tbody>\n</table>"

data Motorregister = MkMotorregister { _brand :: Maybe T.Text
                                     , _year :: Maybe T.Text
                                     , _license :: Maybe T.Text
                                     } deriving (Eq, Show, Read)

makeLenses ''Motorregister

data Document = MkDocument { _date :: Maybe T.Text
                           , _mortgage :: Maybe T.Text
                           , _documentType :: Maybe T.Text
                           , _principal :: Maybe T.Text
                           , _rateOfInterest :: Maybe T.Text
                           } deriving (Eq, Show, Read)

makeLenses ''Document

data Creditor = MkCreditor { _cname :: Maybe T.Text
                           , _cvr :: Maybe T.Text
                           } deriving (Eq, Show, Read)

makeLenses ''Creditor

data Debtor = MkDebtor { _dname :: Maybe T.Text
                       , _cpr :: Maybe T.Text
                       } deriving (Eq, Show, Read)

makeLenses ''Debtor

data AdditionalText = MkAdditionalText { _text :: Maybe T.Text } deriving (Eq, Show, Read)

makeLenses ''AdditionalText

data Tinglysning = MkTinglysning { _motorregister :: Motorregister
                                 , _document :: Document
                                 , _creditor :: Creditor
                                 , _debtor :: Debtor
                                 , _additionalText :: AdditionalText
                                 } deriving (Eq, Show, Read)

makeLenses ''Tinglysning

getTextAfter :: T.Text -> T.Text -> Maybe T.Text
getTextAfter a b = getElementAfter a $ getTagTexts b

getTextsAfter :: T.Text -> T.Text -> [Maybe T.Text]
getTextsAfter a b = getElementsAfter a $ getTagTexts b

cred = MkCreditor { _cname = (Just(T.pack "sdvsddv")), _cvr = (Just(T.pack "vsdvds")) }
--cred = MkCreditor

setCreditor c a = c & (cname .~ (getCreditorName a)) . (cvr .~ (getCreditorCVR a))

getCretitor c = c ^. cname
  
  

getMotorregisterBrand :: T.Text -> Maybe T.Text
getMotorregisterBrand a = getTextAfter (T.pack "Mærke:") a

getMotorregisterYear :: T.Text -> Maybe T.Text
getMotorregisterYear a = getTextAfter (T.pack "Årgang:") a

getMotorregisterLicense :: T.Text -> Maybe T.Text
getMotorregisterLicense a = getTextAfter (T.pack "Registreringsnummer:") a

getDocumentDate :: T.Text -> Maybe T.Text
getDocumentDate a = getTextAfter (T.pack "Dato/løbenummer:") a

getDocumentDocumentType :: T.Text -> Maybe T.Text
getDocumentDocumentType a = getTextAfter (T.pack "Dokument type:") a

getDocumentMortgage :: T.Text -> Maybe T.Text
getDocumentMortgage a = getTextAfter (T.pack "Prioritet:") a

getDocumentPrincipal :: T.Text -> Maybe T.Text
getDocumentPrincipal a = getTextAfter (T.pack "Hovedstol:") a

getDocumentRateOfInterest :: T.Text -> Maybe T.Text
getDocumentRateOfInterest a = getTextAfter (T.pack "Rentesats:") a

getDebtorName :: T.Text -> Maybe T.Text
getDebtorName a = do
  let names = getTextsAfter (T.pack "Navn:") a
  name <- getElementAt names 1
  return $ fromJust name

getDebtorCPR :: T.Text -> Maybe T.Text
getDebtorCPR a = getTextAfter (T.pack "CPR:") a

getCreditorName :: T.Text -> Maybe T.Text
getCreditorName a = do
  let names = getTextsAfter (T.pack "Navn:") a
  name <- getElementAt names 0
  return $ fromJust name

getCreditorCVR :: T.Text -> Maybe T.Text
getCreditorCVR a = getTextAfter (T.pack "CVR:") a



                    
--getAdditionalText :: T.Text -> Maybe T.Text

  
