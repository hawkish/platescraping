{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveGeneric #-}
module SurveyorRapportType (initSurveyorRapport, SurveyorRapport, Surveyor, Vehicle, SurveyorDetails, ErrorOverview, ServiceRemarks) where

import Utils (getElementAfter, getElementsAfter, getElementAt, dequote, getTagTexts, getTextAfter, getTextAfterAt, getTextsAfter, del_every_nth)
import Text.HTML.TagSoup (parseTags, fromTagText, isTagText, isTagOpen, Tag, Tag(TagOpen), (~/=), (~==))
import qualified Text.HTML.TagSoup as TS
import qualified Data.Text as T
import Control.Lens
import GHC.Generics
import Data.List

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

data ErrorOverview = MkErrorOverview { _errorTexts :: [[T.Text]] } deriving (Eq, Show, Read, Generic)

data ServiceRemarks = MkServiceRemarks {_remarks :: [T.Text] } deriving (Eq, Show, Read, Generic)

data SurveyorRapport = MkSurveyorRapport { _surveyor :: Surveyor
                                         , _vehicle :: Vehicle
                                         , _surveyorDetails :: SurveyorDetails
                                         , _errorOverview :: ErrorOverview
                                         , _serviceRemarks :: ServiceRemarks
                                         } deriving (Eq, Show, Read, Generic)


makeLenses ''Surveyor
makeLenses ''Vehicle
makeLenses ''SurveyorDetails
makeLenses ''ErrorOverview
makeLenses ''ServiceRemarks
makeLenses ''SurveyorRapport

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

initErrorOverview a = MkErrorOverview { _errorTexts = getErrorTexts a }

getErrorTexts :: T.Text -> [[T.Text]]
getErrorTexts a = transpose ([getTitles a] ++ [getInformation a])

-- The reverse . del_every_nth .reverse deletes the class=numbers text from the list.
getInformation :: T.Text -> [T.Text]
getInformation = reverse . del_every_nth 2 . reverse . dequote . map extractText . (filter isTagText) . getClassErrorList . parseTags
                 where extractText = T.unwords . T.words . fromTagText

getTitles :: T.Text -> [T.Text]
getTitles = dequote . getTitleAttribs . (filter isTagOpen) . getClassErrorList . parseTags

getTitleAttribs :: [Tag T.Text] -> [T.Text]
getTitleAttribs = map (TS.fromAttrib ("title" :: T.Text))

-- First drop while class=errorList isn't found. Then take while class=clear isn't found.
getClassErrorList :: [Tag T.Text] -> [Tag T.Text]
getClassErrorList = takeWhile (~/= ("<div class=clear>" :: String)) . dropWhile (~/= ("<div class=errorList>" :: String))

initServiceRemarks a = MkServiceRemarks {_remarks = getServiceRemarks a }

getServiceRemarks :: T.Text -> [T.Text]
getServiceRemarks = dequote . map extractText . (filter isTagText) . getClassServiceRemark . parseTags
                   where extractText = T.unwords . T.words . fromTagText

getClassServiceRemark = takeWhile (~/= ("<div class=clear>" :: String)) . dropWhile (~/= ("<div class=serviceRemarkBullet>" :: String))

initSurveyorRapport a = MkSurveyorRapport { _surveyor = initSurveyor a
                                        , _vehicle = initVehicle a
                                        , _surveyorDetails = initSurveyorDetails a
                                        , _errorOverview = initErrorOverview a
                                        , _serviceRemarks = initServiceRemarks a }

