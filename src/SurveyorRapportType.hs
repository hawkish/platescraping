{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveGeneric #-}
module SurveyorRapportType (initSurveyorRapport, SurveyorRapport) where

import Utils (getElementAfter, getElementsAfter, getElementAt, getTagTexts, getTextAfter, getTextsAfter)

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

initSurveyor a = MkSurveyor { _surveyorName = getTextAfter (T.pack "Virksomhed") a
                            , _cvr = getTextAfter (T.pack "CVR") a
                            , _place = getTextAfter (T.pack "Sted") a }

initSurveyorDetails a = MkSurveyorDetails { _surveyorKind = getTextAfter (T.pack "Synsart") a
                                          , _surveyorType = getTextAfter (T.pack "Synstype") a
                                          , _surveyorDate = getTextAfter (T.pack "Synsdato") a
                                          , _endTime = getTextAfter (T.pack "Sluttid") a
                                          , _odometer = getTextAfter (T.pack "Km-stand") a
                                          , _surveyorResult = getTextAfter (T.pack "Synsresultat") a
                                          , _surveyorDeadline = getTextAfter (T.pack "Sidste frist for omsyn/genfremstilling") a }

initVehicle a = MkVehicle { _brand = getTextAfter (T.pack "Mærke") a
                          , _model = getTextAfter (T.pack "Model") a
                          , _vehicleKind = getTextAfter (T.pack "Køretøjsart") a
                          , _registrationNumber = getTextAfter (T.pack "Reg.nr.") a
                          , _vin = getTextAfter (T.pack "Stelnr.") a
                          , _vehicleID = getTextAfter (T.pack "Køretøjs-ID") a }

initErrorOverview a = MkErrorOverview { _errorText = getTextAfter (T.pack "Sted") a }

initServiceRemarks a = MkServiceRemarks {_serviceText = getTextAfter (T.pack "Sted") a }

initSurveyorRapport a = MkSurveyorRapport { _surveyor = initSurveyor a
                                        , _vehicle = initVehicle a
                                        , _surveyorDetails = initSurveyorDetails a
                                        , _errorOverview = initErrorOverview a
                                        , _serviceRemarks = initServiceRemarks a }

--data SurveyorRapports = MkSurveyorRapports { _surveyorRapports :: [SurveyorRapport] } deriving (Eq, Show, Read, Generic)
