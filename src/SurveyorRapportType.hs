{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveGeneric #-}
module SurveyorRapportType (initSurveyorRapport, SurveyorRapport) where

import Utils (getElementAfter, getElementsAfter, getElementAt, dequote, getTagTexts, getTextAfter, getTextAfterAt, getTextsAfter)
import Text.HTML.TagSoup (parseTags, fromTagText, isTagText, isTagOpen, Tag, (~/=))
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

t = T.pack "<h2>Fejloversigt<span class=\"floatRightNoClear infoText hideOnSmall\">Hold musen over tallet for beskrivelse</span><span class=\"floatRightNoClear infoText hideOnLarge hideOnMedium\">Tryk p\229 tallet for beskrivelse</span></h2></div>\r\n\t<div class=\"clear\"></div>\r\n    <div class=\"errorList\">\r\n        \r\n        \r\n                <div class=\"number\" title='Bremser'>5</div>\r\n\t            <div class=\"information\">st\230nksk\230rm, t\230ret, venstre, bag</div>\r\n            \r\n                <div class=\"number\" title='El-anl\230g, lygter, reflekser mv.'>6</div>\r\n\t            <div class=\"information\">nummerpladelygte, virker ikke, venstre</div>\r\n            \r\n    </div>\r\n</div>\r\n\r\n<div class=\"clear\"></div>"

getTitles = dequote . getTitleAttribs . (filter isTagOpen) . getErrorListTags

getTitleAttribs = map (TS.fromAttrib ("title" :: T.Text))

getErrorListTags :: [Tag T.Text] -> [Tag T.Text]
getErrorListTags = dropWhile (~/= ("<div class=errorList>" :: String)) 


initServiceRemarks a = MkServiceRemarks {_serviceText = getTextAfterAt (T.pack "Sted") 2 a }

initSurveyorRapport a = MkSurveyorRapport { _surveyor = initSurveyor a
                                        , _vehicle = initVehicle a
                                        , _surveyorDetails = initSurveyorDetails a
                                        , _errorOverview = initErrorOverview a
                                        , _serviceRemarks = initServiceRemarks a }

--data SurveyorRapports = MkSurveyorRapports { _surveyorRapports :: [SurveyorRapport] } deriving (Eq, Show, Read, Generic)
