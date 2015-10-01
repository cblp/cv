{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import Control.Monad
import Data.ByteString.Lazy as ByteString
import Data.CV
import Text.Blaze.Html5 ( (!), a, li, p, ul )
import Text.Blaze.Html5.Attributes

main :: IO ()
main = do
    let cv = CV{..}
    ByteString.writeFile "mycv.en.html" (renderCv En cv)
    ByteString.writeFile "mycv.ru.html" (renderCv Ru cv)
  where
    fullname En = "Yuriy Syrovetskiy"
    fullname Ru = "Юрий Сыровецкий"

    photo = "Yuriy_Syrovetskiy.jpg"

    contactInfo = [ Telephone     "+7 905 547 11 98"
                  , Skype         "cblp.su"
                  , EMail         "cblp@cblp.su"
                  -- , Jabber        "cblp@cblp.su"
                  , Telegram      "cblp_su"
                  , PersonalPage  "http://" "cblp.su"
                  , GitHub        "cblp"
                  , Bitbucket     "cblp"
                  , LinkedIn      "cblpsu"
                  , Facebook      "cblp.su"
                  , Twitter       "cblp_su"
                  ]

    professionalSkills En = ul $ do
        li "Desktop and server (backend) programming. Data analysis, high load services, user interface design."
        li "Coding, project management, deployment, staff training."
    professionalSkills Ru = ul $ do
        li "Десктопное и серверное (backend) программирование. Анализ данных, высокие нагрузки, пользовательский интерфейс."
        li "Кодирование, управление проектом, внедрение, обучение персонала."

    technologies =
        [ ( "I am good in", [ "C", "C++", "English", "git", "Haskell"
                            , "Linux [Debian, Ubuntu]", "Mercurial", "Python"
                            , "Qt", "Russian", "Subversion" ] )
        , ( "I can use",    [ "Boost", "HTML", "JavaScript", "Java", "Perl"
                            , "PHP", "Windows", "XML" ] )
        , ( "I can read",   [ "Assembler", "Erlang", ".NET/C#"
                            , "LISP/Clojure/Scheme", "Ruby", "Scala"
                            , "Smalltalk", "other cool stuff" ] )
        ]

    workExperience =
        [ Work  { workStart = (2015, Jan), workEnd = Nothing, totalTime = "1 semester"
                , organization = "The Moscow Chemical Lyceum"
                , location = "Moscow, Russia"
                , position = "teacher of functional programming (Haskell)"
                , description = pure ()
                }
        , Work  { workStart = (2012, Sep), workEnd = Nothing, totalTime = "3 years"
                , organization = "The Moscow Chemical Lyceum"
                , location = "Moscow, Russia"
                , position = "student scientific works mentor"
                , description = pure ()
                }
        , Work  { workStart = (2011, Dec), workEnd = Nothing, totalTime = "4 years"
                , organization = "Yandex"
                , location = "Moscow, Russia"
                , position = "software developer"
                , description = do
                      p $ do
                          void "I'm a backend developer of the keyword statistics service "
                          a ! href "http://wordstat.yandex.com/" $ "Wordstat.yandex.com"
                          " and several internal Yandex services."
                      p "My software successfully stands year-to-year growing data and user traffic."
                }
        , Work  { workStart = (2006, Nov), workEnd = Just (2011, Oct), totalTime = "5 years"
                , organization = "Research Institute of Information Technologies"
                , location = "Moscow, Russia"
                , position = "engineer"
                , description = do
                      p "I was the lead developer of multi-component software system."
                      p "I've been working on design and code, program and user documentation, deploy and customer support."
                      p "In my team, I introduced usage of source control tools, issue management, common knowledge system (wiki)."
                }
        ]

    education =
        [ Education { graduated = 2010
                    , school = "The Moscow Institute of Humanities and Economics"
                    , division = "faculty of law"
                    , degree = "higher/specialist in jurisprudence, civil law"
                    }
        , Education { graduated = 2006
                    , school = "Institute of Cryptography, Communications and Informatics"
                    , division = "faculty of information security"
                    , degree = "incomplete higher in computer security"
                    }
        , Education { graduated = 2002
                    , school = "The Moscow Chemical Lyceum"
                    , division = "faculty of physics and mathematics"
                    , degree = "secondary"
                    }
        ]

    achievements =
        [ ( 2015, Jun
          , do  p $ do
                    void "Organized Haskell meetup/conference in Moscow, Russia: 6 talks, 50+ attendees"
                    void " (schedule in Russian: "
                    a ! href "https://github.com/ruHaskell/ruhaskell/wiki/Meetup2015Summer" $
                        "github.com/ruHaskell/ruhaskell/wiki/Meetup2015Summer"
                    ")."
                p "Gave a talk “Haskell for pythonists” there."
          )
        ]

    residence = do
        p "Moscow, Russia."
        p "Ready to relocate."
