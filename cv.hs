{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards #-}

import Control.Monad                ( forM_ )
import Data.ByteString.Lazy         as ByteString ( writeFile )
import Data.CV
import Data.Monoid                  ( (<>) )
import Data.Tuple.X                 ( (-:) )
import GitHubPages                  ( deploy )
import System.Directory             ( copyFile )
import System.FilePath              ( (</>) )
import Text.Blaze.Html5             ( (!), Html, a, p )
import Text.Blaze.Html5.Attributes  ( href )

main :: IO ()
main = deploy build
  where
    cv = CV{..}

    fullname En = "Yuriy Syrovetskiy"
    fullname Ru = "Юрий Сыровецкий"

    photo = "Yuriy_Syrovetskiy.jpg"

    contactInfo = [ Telephone "+7 905 547 11 98"
                  , Skype     "cblp.su"
                  , EMail     "cblp@cblp.su"
                  -- , Jabber    "cblp@cblp.su"
                  , Telegram  "cblp_su"
                  , Personal  "http://" "cblp.su"
                  , GitHub    "cblp"
                  , Bitbucket "cblp"
                  , LinkedIn  "cblpsu"
                  , Facebook  "cblp.su"
                  , Twitter   "cblp_su"
                  ]

    professionalSkills En = do
      p "Desktop and server (backend) programming. Data analysis, high load services, user interface design."
      p "Coding, project management, deployment, staff training."
    professionalSkills Ru = do
      p "Десктопное и серверное (backend) программирование. Анализ данных, высокие нагрузки, пользовательский интерфейс."
      p "Кодирование, управление проектом, внедрение, обучение персонала."

    technologies =
      [ (\case En -> "I am good in"; Ru -> "Владею") -:
        [ "C", "C++", "English", "git", "Haskell", "Linux [Debian, Ubuntu]"
        , "Mercurial", "Python", "Qt"
        , \case En -> "Russian"; Ru -> "Русским", "Subversion"
        ]
      , (\case En -> "I can use"; Ru -> "Разбираюсь в") -:
        [ "Boost", "HTML", "JavaScript", "Java", "Perl", "PHP", "Windows"
        , "XML"
        ]
      , (\case En -> "I can read"; Ru -> "Знаком с") -:
        [ "Assembler", "Erlang", ".NET/C#", "LISP/Clojure/Scheme", "Ruby"
        , "Scala", "Smalltalk"
        , \case En -> "other cool stuff"; Ru -> "другими крутыми штуками"
        ]
      ]

    workExperience =
      [ Work
        { workStart = (2015, Jan), workEnd = Nothing
        , totalTime = tr "1 semester"
        , organization = moscowChemicalLyceum
        , location = moscow
        , position = \case
            En -> "teacher of functional programming (Haskell)"
            Ru -> "преподаватель функционального программирования (Haskell)"
        , description = ""
        }
      , Work
        { workStart = (2012, Sep), workEnd = Nothing
        , totalTime = tr "3 years"
        , organization = moscowChemicalLyceum
        , location = moscow
        , position = \case
            En -> "student scientific works mentor"
            Ru -> "руководитель научных работ школьников"
        , description = ""
        }
      , Work
        { workStart = (2011, Dec), workEnd = Nothing
        , totalTime = tr "4 years"
        , organization = \case En -> "Yandex"; Ru -> "Яндекс"
        , location = moscow
        , position = \case  En -> "software developer"
                            Ru -> "разработчик"
        , description = \case
            En -> do
              p $ do
                "I'm a backend developer of the keyword statistics service " :: Html
                a ! href "http://wordstat.yandex.com/" $
                  "Wordstat.yandex.com"
                " and several internal Yandex services."
              p "My software successfully stands year-to-year growing data and user traffic."
            Ru -> do
              p $ do
                "Разработчик серверной части сервиса статистики ключевых слов " :: Html
                a ! href "http://wordstat.yandex.ru/" $
                  "Wordstat.yandex.ru"
                " и некоторых внутренних сервисов Яндекса."
              p "Мои сервисы успешно справляются с растущей год от года нагрузкой."
        }
      , Work
        { workStart = (2006, Nov), workEnd = Just (2011, Oct)
        , totalTime = tr "5 years"
        , organization = \case
            En -> "Research Institute of Information Technologies"
            Ru -> "НИИ информационных технологий"
        , location = moscow
        , position = \case En -> "engineer"; Ru -> "инженер"
        , description = \case
            En -> do
              p "I was the lead developer of multi-component software data transfer and processing system."
              p "I've been working on design and code, program and user documentation, deploy and customer support."
              p "In my team, I introduced usage of source control tools, issue management, common knowledge system (wiki)."
            Ru -> do
              p "Был ведущим разработчиком многокомпонентной системы передачи и обработки данных."
              p "Занимался проектированием, разработкой, составлением программной и пользовательской документации, внедрением системы в производстве, обучением пользователей и поддержкой."
              p "Внедрил в команде систему управления исходным кодом (Subversion и позже Mercurial), учёт задач, базу знаний (вики)."
        }
      ]

    education =
      [ Education
        { graduated = 2010
        , school = \case
            En -> "The Moscow Institute of Humanities and Economics"
            Ru -> "Московский гуманитарно-экономический институт"
        , division = \case En -> "faculty of law"; Ru -> "юридический факультет"
        , degree = \case
            En -> "higher/specialist in jurisprudence, civil law"
            Ru -> "высшее/специалист, юриспруденция, гражданское право"
        }
      , Education
        { graduated = 2006
        , school = \case
            En -> "Institute of Cryptography, Communications and Informatics"
            Ru -> "Институт криптографии, связи и информатики"
        , division = \case  En -> "faculty of information security"
                            Ru -> "факультет информационной безопасности"
        , degree = \case  En -> "incomplete higher in computer security"
                          Ru -> "неполное высшее, компьютерная безопасность"
        }
      , Education
        { graduated = 2002
        , school = moscowChemicalLyceum
        , division = \case  En -> "faculty of physics and mathematics"
                            Ru -> "физико-математический факультет"
        , degree = \case En -> "secondary"; Ru -> "среднее"
        }
      ]

    achievements =
      [ (2015, Jun) -:
        \case
          En -> do
            p $ do
              "Organized Haskell meetup/conference in Moscow, Russia: 6 talks, 50+ attendees (schedule in Russian: " :: Html
              a ! href "https://github.com/ruHaskell/ruhaskell/wiki/Meetup2015Summer" $
                "github.com/ruHaskell/ruhaskell/wiki/Meetup2015Summer"
              ")."
            p "Gave a talk “Haskell for pythonists” there."
          Ru -> do
            p $ do
              "Организовал встречу-конференцию о языке Haskell в Москве. 6 докладов, больше 50 слушателей (список докладов: " :: Html
              a ! href "https://github.com/ruHaskell/ruhaskell/wiki/Meetup2015Summer" $
                "github.com/ruHaskell/ruhaskell/wiki/Meetup2015Summer"
              ")."
            p "Выступил там с докладом «Haskell для питониста»."
      ]

    residence loc = do
      p $ moscow loc
      p "Ready to relocate."

    moscow En = "Moscow, Russia"
    moscow Ru = "Москва"

    moscowChemicalLyceum En = "The Moscow Chemical Lyceum (School 1303)"
    moscowChemicalLyceum Ru = "Московский Химический Лицей (школа 1303)"

    tr en En = en
    tr en Ru = case en of
      "1 semester" -> "1 семестр"
      "3 years" -> "3 года"
      "4 years" -> "4 года"
      "5 years" -> "5 лет"
      _ -> en

    build target = do
        forM_ allValues $ \locale -> do
            let filename = target </> "cv." <> show locale <> ".html"
            ByteString.writeFile filename (renderCv locale cv)
        copyFile photo (target </> photo)

allValues :: (Bounded a, Enum a) => [a]
allValues = enumFrom minBound
