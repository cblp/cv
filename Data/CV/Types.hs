module Data.CV.Types where

import Text.Blaze.Html

data Locale = En | Ru

type Localized a = Locale -> a

data ContactInfo  = Bitbucket String
                  | EMail String
                  | Facebook String
                  | GitHub String
                  | LinkedIn String
                  | PersonalPage  String -- ^ prefix
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
              , professionalSkills :: Html
              , technologies :: [(String, [String])]
                -- ^ lists of technologies in sections
              , workExperience :: [Work]
              , education :: [Education]
              , achievements :: [(Year, Month, Html)]
              , residence :: Html
              }
