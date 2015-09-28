module Data.CV.Types where

data Language = En | Ru

type Localized a = Language -> a

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
                  , description :: [String]
                  }

data Education = Education  { graduated :: Year
                            , school :: String
                            , division :: String
                            , degree :: String
                            }

type TextBlock = [String]

data CV = CV  { name :: Localized String
              , photo :: String
              , contacts :: [Contact]
              , professionalSkills :: TextBlock
              , technologies :: [(String, TextBlock)]
                -- ^ lists of technologies in sections
              , workExperience :: [Work]
              , education :: [Education]
              , achievements :: [(Year, Month, TextBlock)]
              , residence :: TextBlock
              }
