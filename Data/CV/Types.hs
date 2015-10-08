{-# LANGUAGE FlexibleInstances, TypeOperators #-}

module Data.CV.Types where

import Data.String      ( IsString(..) )
import Data.Tuple.X     ( (:-) )
import Text.Blaze.Html  ( Html )

data Locale = En | Ru

type Localized a = Locale -> a

instance IsString string => IsString (Localized string) where
    fromString = const . fromString

data ContactInfo  = Bitbucket String
                  | EMail String
                  | Facebook String
                  | GitHub String
                  | LinkedIn String
                  | Personal  String -- ^ prefix
                              String -- ^ URL without prefix
                  | Skype String
                  | Telegram String
                  | Telephone String
                  | Twitter String
    deriving Show

type Year = Int

data Month =
    Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
    deriving Show

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

data Work = Work  { workStart :: (Year, Month)
                  , workEnd :: Maybe (Year, Month)
                  , totalTime :: Localized String
                  , organization :: Localized String
                  , location :: Localized String
                  , position :: Localized String
                  , description :: Localized Html
                  }

data Education = Education  { graduated :: Year
                            , school :: Localized String
                            , division :: Localized String
                            , degree :: Localized String
                            }

data CV = CV  { fullname :: Localized String
              , photo :: FilePath
              , contactInfo :: [ContactInfo]
              , professionalSkills :: Localized Html
              , technologies :: [Localized String :- [Localized String]]
                -- ^ lists of technologies in groups
              , workExperience :: [Work]
              , education :: [Education]
              , achievements :: [(Year, Month) :- Localized Html]
              , residence :: Localized Html
              }
