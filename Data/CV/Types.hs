{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Data.CV.Types where

import           Data.Tuple.X ((:-))
import           Text.Blaze.Html (Html)

data ContactInfo
    = Bitbucket String
    | EMail String
    | Facebook String
    | GitHub String
    | LinkedIn String
    | Personal String -- ^ prefix
               String -- ^ URL without prefix
    | Skype String
    | Telegram String
    | Telephone String
    | Twitter String
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

showRu :: Month -> String
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
    , totalTime    :: String
    , organization :: String
    , location     :: String
    , position     :: String
    , description  :: Html
    }

data Education = Education
    { graduated :: Year
    , school    :: String
    , division  :: String
    , degree    :: String
    }

data CV = CV
    { fullname           :: String
    , photo              :: FilePath
    , contactInfo        :: [ContactInfo]
    , professionalSkills :: Html
    , technologies       :: [String :- [String]]
      -- ^ lists of technologies in groups
    , workExperience     :: [Work]
    , education          :: [Education]
    , publicActivity     :: [(Year, Month) :- Html]
    , talks              :: [(Year, Month) :- Html]
    , residence          :: Html
    }
