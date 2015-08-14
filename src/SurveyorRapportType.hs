{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveGeneric #-}
module SurveyorRapportType () where

import Utils (getElementAfter, getElementsAfter, getElementAt, getTagTexts, getTextAfter, getTextsAfter)
import Data.Maybe
import qualified Data.Text as T
import Control.Lens
import GHC.Generics

data Surveyor = MkSurveyor { _surveyorName
                           , _cvr
                           , _place
                           } deriving (Eq, Show, Read, Generic)

data Vehicle = MkVehicle { _brand :: Maybe T.Text
                         ,  _model :: Maybe T.Text
                         ,  _vehiclekind :: Maybe T.Text
                         ,  _registrationNumber :: Maybe T.Text
                         ,  _vin :: Maybe T.Text
                         ,  _vehicleID :: Maybe T.Text
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

data ServiceRemarks = MkServiceRemarks {_serviceRemarks :: Maybe T.Text } deriving (Eq, Show, Read, Generic)

data SurveyorRapport = MkSurveyorRapport { _surveyor :: Surveyor
                                         , _vehicle :: Vehicle
                                         , _surveyorDetails :: SurveyorDetails
                                         , _errorOverview :: ErrorOverview
                                         , _systemRemarks :: ServiceRemarks
                                         } deriving (Eq, Show, Read, Generic)

data SurveyorRapports = MkSurveyorRapports { _surveyorRapports :: [SurveyorRapport] } deriving (Eq, Show, Read, Generic)

makeLenses ''Surveyor
makeLenses ''Vehicle
makeLenses ''SurveyorDetails
makeLenses ''ErrorOverview
makeLenses ''ServiceRemarks
makeLenses ''SurveyorRapport
makeLenses ''SurveyorRapports
