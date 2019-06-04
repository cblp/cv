{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Data.CV.Types where

import           Data.Text (Text)
import           Data.Tuple.X ((:-))
import           Text.Blaze.Html (Html)

data ContactInfo
    = Bitbucket Text
    | EMail Text
    | Facebook Text
    | GitHub Text
    | LinkedIn Text
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

data Work = Work
    { workStart    :: (Year, Month)
    , workEnd      :: Maybe (Year, Month)
    , totalTime    :: Text
    , organization :: Text
    , location     :: Text
    , position     :: Text
    , description  :: Html
    }

data Education = Education
    { graduated :: Year
    , school    :: Text
    , division  :: Text
    , degree    :: Text
    }

data CV = CV
    { fullname       :: Text
    , photo          :: FilePath
    , contactInfo    :: [ContactInfo]
    , competencies   :: Html
    , technologies   :: [Text]
    , workExperience :: [Work]
    , education      :: [Education]
    , publicActivity :: [(Year, Month) :- Html]
    , talks          :: [(Year, Month) :- Html]
    , residence      :: Html
    }
