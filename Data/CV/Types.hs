{-# LANGUAGE FlexibleInstances #-}

module Data.CV.Types where

import Data.String
import Text.Blaze.Html

data Locale = En | Ru

newtype Localized a = Localized (Locale -> a)

instance IsString (Localized String) where
    fromString = Localized . const

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

data Work = Work  { workStart :: (Year, Month)
                  , workEnd :: Maybe (Year, Month)
                  , totalTime :: String
                  , organization :: String
                  , location :: String
                  , position :: String
                  , description :: Html
                  }

data Education = Education  { graduated :: Year
                            , school :: String
                            , division :: String
                            , degree :: String
                            }

data CV = CV  { fullname :: Localized String
              , photo :: String
              , contactInfo :: [ContactInfo]
              , professionalSkills :: Localized Html
              , technologies :: [(Localized String, [Localized String])]
                -- ^ lists of technologies in sections
              , workExperience :: [Work]
              , education :: [Education]
              , achievements :: [(Year, Month, Html)]
              , residence :: Html
              }
