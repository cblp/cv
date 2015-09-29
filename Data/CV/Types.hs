module Data.CV.Types where

import Text.Blaze.Html

data Scheme = Callto | Http | Https | Mailto | WebTg

instance Show Scheme where
    show Callto = "callto"
    show Http   = "http"
    show Https  = "https"
    show Mailto = "mailto"
    show WebTg  = "web+tg"

instance ToMarkup Scheme where
    toMarkup = toMarkup . show

data Locale = En | Ru

type Localized a = Locale -> a

data ContactInfo  = Bitbucket String
                  | EMail String
                  | Facebook String
                  | GitHub String
                  | LinkedIn String
                  | PersonalPage  Scheme
                                  String -- ^ URL without scheme
                  | Skype String
                  | Telegram String
                  | Telephone String
                  | Twitter String
    deriving Show

type Year = Int

data Month =
    Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

data Work = Work  { start :: (Year, Month)
                  , end :: Maybe (Year, Month)
                  , totalTime :: String
                  , organization :: String
                  , location :: String
                  , title :: String
                  , description :: [String]
                  }

data Education = Education  { graduated :: Year
                            , school :: String
                            , division :: String
                            , degree :: String
                            }

type TextBlock = [String]

data CV = CV  { fullname :: Localized String
              , photo :: String
              , contactInfo :: [ContactInfo]
              , professionalSkills :: TextBlock
              , technologies :: [(String, TextBlock)]
                -- ^ lists of technologies in sections
              , workExperience :: [Work]
              , education :: [Education]
              , achievements :: [(Year, Month, TextBlock)]
              , residence :: TextBlock
              }
