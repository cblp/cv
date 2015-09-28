module Data.CV where

data Localized a = Localized { en :: a, ru :: a }

data Contact  = Bitbucket String
              | EMail String
              | Facebook String
              | GitHub String
              | LinkedIn String
              | PersonalPage String
              | Skype String
              | Telegram String
              | Telephone String
              | Twitter String

type Year = Int

data Month =
    Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

data Work = Work  { start :: (Year, Month)
                  , end :: Maybe (Year, Month)
                  , totalTime :: String
                  , organization :: String
                  , location :: String
                  , title :: String
                  , description :: String
                  }

data Education = Education  { graduated :: Year
                            , school :: String
                            , division :: String
                            , degree :: String
                            }

data CV = CV { name :: String }
