{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.ByteString.Lazy        as ByteString (writeFile)
import Data.Foldable               (for_)
import Data.Monoid                 ((<>))
import System.Directory            (copyFile, createDirectoryIfMissing)
import System.Environment          (getArgs)
import System.FilePath             ((</>))
import Text.Blaze.Html5            (Html, a, p, (!))
import Text.Blaze.Html5.Attributes (href)

import Data.CV.Render (renderCv)
import Data.CV.Types  (CV(..), ContactInfo(..), Education(..), Locale(En, Ru),
                       Month(..), Work(..))
import Data.Tuple.X   ((-:))
import GitHubPages    (deploy)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-d"] -> deploy build
        [] -> build "_site"
        _ -> error "don't know what to do"
  where
    cv = CV {..}
    fullname En = "Yuriy Syrovetskiy"
    fullname Ru = "Юрий Сыровецкий"
    photo = "Yuriy_Syrovetskiy.jpg"
    contactInfo =
        [ Telephone "+7 905 547 11 98"
        , Skype "cblp.su"
        , EMail "cblp@cblp.su"
        , Telegram "cblp_su"
        , Personal "http://" "cblp.su"
        , GitHub "cblp"
        , Bitbucket "cblp"
        , LinkedIn "cblpsu"
        , Facebook "cblp.su"
        , Twitter "cblp_su"
        ]
    professionalSkills En = do
        p
            "Desktop and server (backend) programming. Data analysis, high load services, user interface design."
        p "Design, coding, project management, deployment, staff training."
    professionalSkills Ru = do
        p
            "Десктопное и серверное (backend) программирование. Анализ данных, высокие нагрузки, пользовательский интерфейс."
        p
            "Проектирование, кодирование, управление проектом, внедрение, обучение персонала."
    technologies =
        [ (\case
               En -> "I am good in"
               Ru -> "Владею") -:
          [ "C"
          , "C++"
          , "English"
          , "git"
          , "Haskell"
          , "Linux [Debian, Ubuntu]"
          , "Mercurial"
          , "Python"
          , "Qt"
          , \case
                En -> "Russian"
                Ru -> "Русским"
          , "Subversion"
          ]
        , (\case
               En -> "I can use"
               Ru -> "Разбираюсь в") -:
          [ "Boost"
          , "HTML"
          , "JavaScript"
          , "Java"
          , "Perl"
          , "PHP"
          , "Scala"
          , "Windows"
          , "XML"
          ]
        , (\case
               En -> "I can read"
               Ru -> "Знаком с") -:
          [ "Assembler"
          , "Erlang"
          , ".NET/C#"
          , "LISP/Clojure/Scheme"
          , "Ruby"
          , "Smalltalk"
          , \case
                En -> "other cool stuff"
                Ru -> "другими крутыми штуками"
          ]
        ]
    workExperience =
        [ Work
          { workStart = (2016, Feb)
          , workEnd = Nothing
          , totalTime = tr "¹⁄₂ year"
          , organization =
                \case
                    En -> "Kaspersky Lab"
                    Ru -> "Лаборатория Касперского"
          , location = moscow
          , position =
                \case
                    En -> "developer"
                    Ru -> "разработчик"
          , description =
                \case
                    En ->
                        "As a Kaspersky OS and Kaspersky Security System development team member, I implement security configuration compiler and various security policies, using Haskell (primarily) and C."
                    Ru ->
                        "В команде разработки Kaspersky OS и безопасной платформы Kaspersky Security System я разрабатываю транслятор для языка конфигурации безопасности и реализую различные политики безопасности, используя языки Haskell (большей частью) и C."
          }
        , Work
          { workStart = (2012, Sep)
          , workEnd = Nothing
          , totalTime = tr "4 years"
          , organization = moscowChemicalLyceum
          , location = moscow
          , position =
                \case
                    En -> "student scientific works mentor"
                    Ru -> "руководитель научных работ школьников"
          , description = ""
          }
        , Work
          { workStart = (2011, Dec)
          , workEnd = Just (2016, Feb)
          , totalTime = tr "4 years"
          , organization =
                \case
                    En -> "Yandex"
                    Ru -> "Яндекс"
          , location = moscow
          , position =
                \case
                    En -> "software developer"
                    Ru -> "разработчик"
          , description =
                \case
                    En -> do
                        p $ do
                            "I'm a backend developer of the keyword statistics service " :: Html
                            a ! href "http://wordstat.yandex.com/" $
                                "Wordstat.yandex.com"
                            " and several internal Yandex services."
                        p
                            "My software successfully stands year-to-year growing data and user traffic."
                    Ru -> do
                        p $ do
                            "Разработчик серверной части сервиса статистики ключевых слов " :: Html
                            a ! href "http://wordstat.yandex.ru/" $
                                "Wordstat.yandex.ru"
                            " и некоторых внутренних сервисов Яндекса."
                        p
                            "Мои сервисы успешно справляются с растущей год от года нагрузкой."
          }
        , Work
          { workStart = (2015, Jan)
          , workEnd = Just (2015, Jun)
          , totalTime = tr "1 semester"
          , organization = moscowChemicalLyceum
          , location = moscow
          , position =
                \case
                    En -> "teacher of functional programming (Haskell)"
                    Ru ->
                        "преподаватель функционального программирования (Haskell)"
          , description =
                \case
                    En ->
                        p
                            "Optional subject for 9th, 10th and 11th grade students."
                    Ru -> p "Факультатив для 9, 10 и 11 классов."
          }
        , Work
          { workStart = (2006, Nov)
          , workEnd = Just (2011, Oct)
          , totalTime = tr "5 years"
          , organization =
                \case
                    En -> "Research Institute of Information Technologies"
                    Ru -> "НИИ информационных технологий"
          , location = moscow
          , position =
                \case
                    En -> "engineer"
                    Ru -> "инженер"
          , description =
                \case
                    En -> do
                        p
                            "I was the lead developer of multi-component software data transfer and processing system."
                        p
                            "I've been working on design and code, program and user documentation, deploy and customer support."
                        p
                            "In my team, I introduced usage of source control tools, issue management, common knowledge system (wiki)."
                    Ru -> do
                        p
                            "Был ведущим разработчиком многокомпонентной системы передачи и обработки данных."
                        p
                            "Занимался проектированием, разработкой, составлением программной и пользовательской документации, внедрением системы в производстве, обучением пользователей и поддержкой."
                        p
                            "Внедрил в команде систему управления исходным кодом (Subversion и позже Mercurial), учёт задач, базу знаний (вики)."
          }
        ]
    education =
        [ Education
          { graduated = 2010
          , school =
                \case
                    En -> "The Moscow Institute of Humanities and Economics"
                    Ru -> "Московский гуманитарно-экономический институт"
          , division =
                \case
                    En -> "faculty of law"
                    Ru -> "юридический факультет"
          , degree =
                \case
                    En -> "higher/specialist in jurisprudence, civil law"
                    Ru -> "высшее/специалист, юриспруденция, гражданское право"
          }
        , Education
          { graduated = 2006
          , school =
                \case
                    En ->
                        "Institute of Cryptography, Communications and Informatics"
                    Ru -> "Институт криптографии, связи и информатики"
          , division =
                \case
                    En -> "faculty of information security"
                    Ru -> "факультет информационной безопасности"
          , degree =
                \case
                    En -> "incomplete higher in computer security"
                    Ru -> "неполное высшее, компьютерная безопасность"
          }
        , Education
          { graduated = 2002
          , school = moscowChemicalLyceum
          , division =
                \case
                    En -> "faculty of physics and mathematics"
                    Ru -> "физико-математический факультет"
          , degree =
                \case
                    En -> "secondary"
                    Ru -> "среднее"
          }
        ]
    publicActivity =
        [ (2016, Aug) -: \loc ->
              p $ do
                  case loc of
                      En -> do
                          "Organized RuHaskell community meetup in Moscow, Russia. " :: Html
                          "6 talks, 50+ attendees. Schedule (Russian): " :: Html
                      Ru -> do
                          "Организовал митап сообщества RuHaskell в Москве. " :: Html
                          "6 докладов, больше 50 слушателей. Список докладов: " :: Html
                  a !
                      href
                          "http://ruhaskell.org/posts/events/2015/11/05/meetup-winter-register.html" $
                      "ruhaskell.org/posts/events/2015/11/05/meetup-winter-register.html"
        , (2015, Dec) -: \loc ->
              p $ do
                  case loc of
                      En -> do
                          "Organized RuHaskell community meetup in Moscow, Russia. " :: Html
                          "6 talks, 50+ attendees. Schedule (Russian): " :: Html
                      Ru -> do
                          "Организовал митап сообщества RuHaskell в Москве. " :: Html
                          "6 докладов, больше 50 слушателей. Список докладов: " :: Html
                  a !
                      href
                          "http://ruhaskell.org/posts/events/2015/11/05/meetup-winter-register.html" $
                      "ruhaskell.org/posts/events/2015/11/05/meetup-winter-register.html"
        , (2015, Jun) -: \loc ->
              p $ do
                  case loc of
                      En -> do
                          "Organized RuHaskell community meetup in Moscow, Russia. " :: Html
                          "6 talks, 50+ attendees. Schedule (Russian): " :: Html
                      Ru -> do
                          "Организовал митап сообщества RuHaskell в Москве. " :: Html
                          "6 докладов, больше 50 слушателей. Список докладов: "
                  a !
                      href
                          "https://github.com/ruHaskell/ruhaskell/wiki/Meetup2015Summer" $
                      "github.com/ruHaskell/ruhaskell/wiki/Meetup2015Summer"
        ]
    talks =
        [ (2015, Oct) -: \loc ->
              p $ do
                  case loc of
                      En -> do
                          "“Haskell for Pythonista” at the Python&Admin Party in Novosibirsk. " :: Html
                          "Description (Russian): " :: Html
                      Ru -> do
                          "«Haskell для питониста» на митапе Python&Admin Party в Новосибирске. " :: Html
                          "Описание: " :: Html
                  a ! href "https://events.yandex.ru/lib/talks/3223/" $
                      "events.yandex.ru/lib/talks/3223"
        , (2015, Jun) -: \loc ->
              p $ do
                  case loc of
                      En -> do
                          "“Haskell for Pythonista” at the RuHaskell community Meetup in Moscow. " :: Html
                          "Video (Russian): " :: Html
                      Ru -> do
                          "«Haskell для питониста» на московском митапе сообщества RuHaskell. " :: Html
                          "Видео: " :: Html
                  a !
                      href
                          "http://ruhaskell.org/posts/talks/2015/06/21/haskell-for-pythonista.html" $
                      "ruhaskell.org/posts/talks/2015/06/21/haskell-for-pythonista.html"
        ]
    residence loc = do
        p $ moscow loc
        p $
            case loc of
                En -> "Able to relocate to Europe or North America."
                Ru -> "Могу переехать в Европу или Северную Америку."
    moscow En = "Moscow, Russia"
    moscow Ru = "Москва"
    moscowChemicalLyceum En = "The Moscow Chemical Lyceum (School 1303)"
    moscowChemicalLyceum Ru = "Московский Химический Лицей (школа 1303)"
    tr en En = en
    tr en Ru =
        case en of
            "1 semester" -> "1 семестр"
            "¹⁄₂ year" -> "¹⁄₂ года"
            "3 years" -> "3 года"
            "4 years" -> "4 года"
            "5 years" -> "5 лет"
            _ -> error $ "not translated: " <> en
    build target = do
        createDirectoryIfMissing True target
        for_ allValues $ \locale -> do
            let filename = target </> "cv." <> show locale <> ".html"
            ByteString.writeFile filename (renderCv locale cv)
        copyFile photo (target </> photo)
        putStrLn $ "built site in " <> show target

allValues
    :: (Bounded a, Enum a)
    => [a]
allValues = enumFrom minBound
