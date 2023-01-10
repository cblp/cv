{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module CV.Types where

import Data.Text (Text)
import Data.Tuple.X ((:-))
import Text.Blaze.Html (Html)

data ContactInfo
    = Bitbucket Text
    | EMail Text
    | Facebook Text
    | GitHub Text
    | LinkedIn Text
    | Location Text
    | Personal Text
    | Skype Text
    | Telegram Text
    | Telephone Text
    | Twitter Text
    deriving (Show)

type Year = Int

data Month
    = Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec
    deriving (Show)

showRu :: Month -> Text
showRu Jan = "январь"
showRu Feb = "февраль"
showRu Mar = "март"
showRu Apr = "апрель"
showRu May = "май"
showRu Jun = "июнь"
showRu Jul = "июль"
showRu Aug = "август"
showRu Sep = "сентябрь"
showRu Oct = "октябрь"
showRu Nov = "ноябрь"
showRu Dec = "декабрь"

data Organization = Freelance | At Text

data Work = Work
    { start         :: (Year, Month)
    , end           :: Maybe (Year, Month)
    , totalTime     :: Text
    , organization  :: Organization
    , location      :: Text
    , position      :: Text
    , description   :: Html
    , toolsAndTechs :: Text
    , visible       :: Bool
    }

data Education = Education
    { graduated   :: Year
    , school      :: Text
    , division    :: Text
    , degree      :: Text
    , description :: Html
    , visible     :: Bool
    }

data CV = CV
    { fullname       :: Text
    , contactInfo    :: [ContactInfo]
    , competencies   :: Html
    , technologies   :: [Html]
    , workExperience :: [Work]
    , education      :: [Education]
    , publicActivity :: [(Year, Month) :- Html]
    , talks          :: [(Year, Month) :- Html]
    }
