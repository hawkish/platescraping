{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveGeneric #-}
module SurveyorRapportType (initSurveyorRapport, SurveyorRapport) where

import Utils (getElementAfter, getElementsAfter, getElementAt, dequote, getTagTexts, getTextAfter, getTextAfterAt, getTextsAfter, isNumeric)
import Text.HTML.TagSoup (parseTags, fromTagText, isTagText, isTagOpen, Tag, (~/=), (~==), sections)
import qualified Text.HTML.TagSoup as TS
import qualified Data.Text as T
import Control.Lens
import GHC.Generics

data Surveyor = MkSurveyor { _surveyorName :: Maybe T.Text
                           , _cvr :: Maybe T.Text
                           , _place :: Maybe T.Text
                           } deriving (Eq, Show, Read, Generic)

data Vehicle = MkVehicle { _brand :: Maybe T.Text
                         , _model :: Maybe T.Text
                         , _vehicleKind :: Maybe T.Text
                         , _registrationNumber :: Maybe T.Text
                         , _vin :: Maybe T.Text
                         , _vehicleID :: Maybe T.Text
                         } deriving (Eq, Show, Read, Generic)

data SurveyorDetails = MkSurveyorDetails { _surveyorKind :: Maybe T.Text
                                         , _surveyorType :: Maybe T.Text
                                         , _surveyorDate :: Maybe T.Text
                                         , _endTime :: Maybe T.Text
                                         , _odometer :: Maybe T.Text
                                         , _surveyorResult :: Maybe T.Text
                                         , _surveyorDeadline :: Maybe T.Text
                                         } deriving (Eq, Show, Read, Generic)

data ErrorOverview = MkErrorOverview { _errorText :: Maybe T.Text } deriving (Eq, Show, Read, Generic)

data ServiceRemarks = MkServiceRemarks {_serviceText :: Maybe T.Text } deriving (Eq, Show, Read, Generic)

data SurveyorRapport = MkSurveyorRapport { _surveyor :: Surveyor
                                         , _vehicle :: Vehicle
                                         , _surveyorDetails :: SurveyorDetails
                                         , _errorOverview :: ErrorOverview
                                         , _serviceRemarks :: ServiceRemarks
                                         } deriving (Eq, Show, Read, Generic)

data SurveyorRapports = MkSurveyorRapports { _surveyorRapports :: [SurveyorRapport] } deriving (Eq, Show, Read, Generic)

makeLenses ''Surveyor
makeLenses ''Vehicle
makeLenses ''SurveyorDetails
makeLenses ''ErrorOverview
makeLenses ''ServiceRemarks
makeLenses ''SurveyorRapport
makeLenses ''SurveyorRapports

initSurveyor a = MkSurveyor { _surveyorName = getTextAfterAt (T.pack "Virksomhed") 2 a
                            , _cvr = getTextAfterAt (T.pack "CVR") 2 a
                            , _place = getTextAfterAt (T.pack "Sted") 2 a }

initSurveyorDetails a = MkSurveyorDetails { _surveyorKind = getTextAfterAt (T.pack "Synsart") 2 a
                                          , _surveyorType = getTextAfterAt (T.pack "Synstype") 2 a
                                          , _surveyorDate = getTextAfterAt (T.pack "Synsdato") 2 a
                                          , _endTime = getTextAfterAt (T.pack "Sluttid") 2 a
                                          , _odometer = getTextAfterAt (T.pack "Km-stand") 2 a
                                          , _surveyorResult = getTextAfterAt (T.pack "Synsresultat") 2 a
                                          , _surveyorDeadline = getTextAfterAt (T.pack "Sidste frist for omsyn/genfremstilling") 2 a }

initVehicle a = MkVehicle { _brand = getTextAfterAt (T.pack "Mærke") 2 a
                          , _model = getTextAfterAt (T.pack "Model") 2 a
                          , _vehicleKind = getTextAfterAt (T.pack "Køretøjsart") 2 a
                          , _registrationNumber = getTextAfterAt (T.pack "Reg.nr.") 2 a
                          , _vin = getTextAfterAt (T.pack "Stelnr.") 2 a
                          , _vehicleID = getTextAfterAt (T.pack "Køretøjs-ID") 2 a }

initErrorOverview a = MkErrorOverview { _errorText = getTextAfterAt (T.pack "Sted") 2 a }

t = T.pack "<div class=\"errorList\">\r\n        \r\n        \r\n                <div class=\"number\" title='Bremser'>5</div>\r\n\t            <div class=\"information\">st\230nksk\230rm, t\230ret, venstre, bag</div>\r\n            \r\n                <div class=\"number\" title='El-anl\230g, lygter, reflekser mv.'>6</div>\r\n\t            <div class=\"information\">nummerpladelygte, virker ikke, venstre</div>\r\n            \r\n    </div>\r\n</div>\r\n\r\n<div class=\"clear\"></div>\r\n<br />\r\n\r\n<div class=\"floatLeft\">\r\n    <h2>Servicebem\230rkninger</h2>\r\n    \r\n    \r\n</div>\r\n\r\n<div class=\"clear\"></div>\r\n<br />\r\n\r\n\t\t\t\t\t</div>\r\n\r\n\r\n\t\t\t\t</div></div></td>\r\n\t\t\t</tr>\r\n\t\t</table></td>\r\n\t</tr>\r\n</table>\r\n\t\t\t\t\t\t\t\t\t\r\n</div>\r\n\t\t\t\t\t\t\t\t<div class=\"rightColumn hideOnMedium hideOnSmall\">\r\n\t\t\t\t\t\t\t\t\t<!-- Main RIGHT content -->\r\n                                    <table width=\"100%\" cellpadding=\"0\" cellspacing=\"0\" border=\"0\">\r\n\t<tr>\r\n\t\t<td id=\"MSOZoneCell_WebPartctl00_m_g_ac7f211e_5fda_4338_9980_57f492a2a0be\" valign=\"top\" class=\"s4-wpcell-plain \"><table class=\"s4-wpTopTable \" border=\"0\" cellpadding=\"0\" cellspacing=\"0\" width=\"100%\">\r\n\t\t\t<tr>\r\n\t\t\t\t<td valign=\"top\"><div WebPartID=\"ac7f211e-5fda-4338-9980-57f492a2a0be\" HasPers=\"false\" id=\"WebPartctl00_m_g_ac7f211e_5fda_4338_9980_57f492a2a0be\" width=\"100%\" class=\"ms-WPBody noindex \" allowDelete=\"false\" allowExport=\"false\" style=\"\" ><div id=\"ctl00_m_g_ac7f211e_5fda_4338_9980_57f492a2a0be\">\r\n\t\t\t\t\t\r\n<div class=\"boxHeader\">\r\n\t<h3>Leder du efter...</h3>\r\n</div>"

getTitles :: T.Text -> [T.Text]
getTitles = dequote . getTitleAttribs . (filter isTagOpen) . getClassErrorList . parseTags

getTitleAttribs :: [Tag T.Text] -> [T.Text]
getTitleAttribs = map (TS.fromAttrib ("title" :: T.Text))

-- Drop while class=errorList isn't found. Then take while class=clear isn't found.
getClassErrorList :: [Tag T.Text] -> [Tag T.Text]
getClassErrorList = takeWhile (~/= ("<div class=clear>" :: String)) . dropWhile (~/= ("<div class=errorList>" :: String))

getInformation = (filter isNumeric) . dequote . map extractText . (filter isTagText) . getClassErrorList . parseTags
                 where extractText = T.unwords . T.words . fromTagText


initServiceRemarks a = MkServiceRemarks {_serviceText = getTextAfterAt (T.pack "Sted") 2 a }

initSurveyorRapport a = MkSurveyorRapport { _surveyor = initSurveyor a
                                        , _vehicle = initVehicle a
                                        , _surveyorDetails = initSurveyorDetails a
                                        , _errorOverview = initErrorOverview a
                                        , _serviceRemarks = initServiceRemarks a }

--data SurveyorRapports = MkSurveyorRapports { _surveyorRapports :: [SurveyorRapport] } deriving (Eq, Show, Read, Generic)
