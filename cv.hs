{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Data.ByteString.Lazy as ByteString (writeFile)
import           Data.Monoid ((<>))
import           System.Directory (copyFile, createDirectoryIfMissing)
import           System.Environment (getArgs)
import           System.FilePath ((</>))
import           Text.Blaze.Html5 (a, p, toHtml, (!))
import           Text.Blaze.Html5.Attributes (href)

import           Data.CV.Render (renderCv)
import           Data.CV.Types (CV (..), ContactInfo (..), Education (..),
                                Month (..), Work (..))
import           Data.Tuple.X ((-:))
import           GitHubPages (deploy)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-d"] -> deploy build
        []     -> build "_site"
        _      -> error "don't know what to do"
  where
    cv = CV{..}

    fullname = "Yuriy Syrovetskiy"

    photo = "Yuriy_Syrovetskiy.jpg"

    contactInfo =
        [ Telephone "+7 905 547 11 98"
        , Skype     "cblp.su"
        , EMail     "cblp@cblp.su"
        , Telegram  "cblp_su"
        , Personal  "http://" "cblp.su"
        , GitHub    "cblp"
        , Bitbucket "cblp"
        , LinkedIn  "cblpsu"
        , Facebook  "cblp.su"
        , Twitter   "cblp_su"
        ]

    competencies = do
        p $ do
            "Backend, Compilers, Data analysis, Distributed systems, "
            "Functional programming, High load, Security, User interface"
        p "Design, Coding, Project management, Deployment, Staff training"

    technologies = ["C/C++", "Haskell", "Linux", "Python"]

    workExperience =
        [ Work
            { workStart = (2016, Feb)
            , workEnd = Nothing
            , totalTime = "3 years"
            , organization = "Kaspersky Lab"
            , location = moscow
            , position = "Senior Developer"
            , description = p
                "As a Kaspersky OS and Kaspersky Security System \
                \development team member, \
                \I implement security configuration compiler and various \
                \security policies, \
                \using Haskell (primarily) and C."
            }
        , Work
            { workStart = (2012, Sep)
            , workEnd = Nothing
            , totalTime = "6 years"
            , organization = moscowChemicalLyceum
            , location = moscow
            , position = "Student scientific projects mentor"
            , description = ""
            }
        , Work
            { workStart =    (2011, Dec)
            , workEnd = Just (2016, Feb)
            , totalTime = "4 years"
            , organization = "Yandex"
            , location = moscow
            , position = "Software Developer"
            , description = do
                p $ do
                    "I was a backend developer of the keyword statistics "
                    "service "
                    a ! href "http://wordstat.yandex.com/" $
                        "Wordstat.yandex.com"
                    " and several internal Yandex services."
                p   "My software successfully stands year-to-year growing \
                    \data and user traffic."
            }
        , Work
            { workStart =    (2015, Jan)
            , workEnd = Just (2015, Jun)
            , totalTime = "1 semester"
            , organization = moscowChemicalLyceum
            , location = moscow
            , position = "Teacher of functional programming (Haskell)"
            , description =
                p "Optional subject for 9th, 10th and 11th grade students."
            }
        , Work
            { workStart =    (2006, Nov)
            , workEnd = Just (2011, Oct)
            , totalTime = "5 years"
            , organization = "Research Institute of Information Technologies"
            , location = moscow
            , position = "Engineer"
            , description = do
                p   "I was the lead developer of multi-component software \
                    \data transfer and processing system."
                p   "I've been working on design and code, program and \
                    \user documentation, deploy and customer support."
                p   "In my team, I introduced usage of source control tools, \
                    \issue management, common knowledge system (wiki)."
            }
        ]

    education =
        [ Education
            { graduated = -2020
            , school = "The Moscow Aviation Institute"
            , division =
                "faculty of control systems and computer science in engineering"
            , degree = "M.S. Student"
            }
        -- , Education
        --     { graduated = 2010
        --     , school = "The Moscow Institute of Humanities and Economics"
        --     , division = "faculty of law"
        --     , degree = "higher/specialist in jurisprudence, civil law"
        --     }
        , Education
            { graduated = 2006
            , school =
                "Institute of Cryptography, Communications and Informatics"
            , division = "faculty of information security"
            , degree = "incomplete higher in computer security"
            }
        -- , Education
        --     { graduated = 2002
        --     , school = moscowChemicalLyceum
        --     , division = "faculty of physics and mathematics"
        --     , degree = "secondary"
        --     }
        ]

    publicActivity =
        [ (2017, Apr) -: p do
            coLaboratoryRuhaskell
            a ! href "https://events.kaspersky.com/event/ruhaskell2" $
                "events.kaspersky.com/event/ruhaskell2"
        , (2016, Aug) -: p do
            coLaboratoryRuhaskell
            a ! href "https://events.kaspersky.com/event/ruhaskell" $
                "events.kaspersky.com/event/ruhaskell"
        , (2015, Dec) -: p do
            ruhaskellExtropolis
            a   ! href
                    "http://ruhaskell.org/posts/events/2015/11/05/\
                    \meetup-winter-register.html"
                $   "ruhaskell.org/posts/events/2015/11/05/\
                    \meetup-winter-register.html"
        , (2015, Jun) -: p do
            ruhaskellExtropolis
            a   ! href
                    "https://github.com/ruHaskell/ruhaskell/wiki/\
                    \Meetup.2015.Summer"
                $ "github.com/ruHaskell/ruhaskell/wiki/Meetup.2015.Summer"
        ]
      where
        coLaboratoryRuhaskell = do
            "Co-organized RuHaskell community meetup "
            "in Kaspersky Lab, Moscow, Russia. "
            "4 talks, 120+ attendees. "
        ruhaskellExtropolis = do
            "Organized RuHaskell community meetup in Moscow, Russia. "
            "6 talks, 50+ attendees. "

    talks =
        [ (2019, May) -: p do
            "“A purely functional approach to CRDT/RON-based "
            "distributed systems” at FPURE. "
            a ! href "https://www.fpure.events" $ "fpure.events"
        -- , (2019, Mar) -: do
        --             "Introduction to Kaspersky Security System and KasperskyOS "
        --             "at “Pi Day: Moscow Programmer Club Meetup.” "
        --     a   ! href
        --             "https://careers.kaspersky.ru/events/\
        --             \moscow-programmer-club-meet-up/"
        --         $ "careers.kaspersky.ru/events/moscow-programmer-club-meet-up"
        -- , (2018, Nov) -: do
        --             "“A practical application of Haskell implementation of "
        --             "CRDT/RON in distributed systems.” "
        --     a ! href "https://spb-fp-meetup.timepad.ru/event/857591/" $
        --         "spb-fp-meetup.timepad.ru/event/857591"
        , (2018, Sep) -: p do
            "“Purely functional programming and KasperskyOS” "
            "in Information Security section at RIFTECH. "
            a ! href "http://tech.rif.ru" $ "tech.rif.ru"
        , (2017, Dec) -: p do
            "(in collaboration with my student Nikolay Loginov) "
            "“CRDT — correctly replicated data in Haskell” "
            "at the functional programming conference FPCONF. "
            "Description: "
            a ! href "http://fpconf.ru/2017.html" $ "fpconf.ru/2017.html"
            ", video: "
            a ! href "https://youtu.be/VFx0H2p3g6c" $ "youtu.be/VFx0H2p3g6c"
        -- , (2016, Aug) -: do
        --             "“Ivory: safe and performant Haskell” "
        --             "at the RuHaskell community meetup in Moscow. "
        --             "Description (Russian): "
        --     a ! href "https://events.kaspersky.com/event/ruhaskell" $
        --         "events.kaspersky.com/event/ruhaskell"
        -- , (2015, Oct) -: do
        --             "“Haskell for Pythonista” "
        --             "at the Python&Admin Party in Novosibirsk. "
        --             "Description (Russian): "
        --     a ! href "https://events.yandex.ru/lib/talks/3223/" $
        --         "events.yandex.ru/lib/talks/3223"
        -- , (2015, Jun) -: do
        --             "“Haskell for Pythonista” "
        --             "at the RuHaskell community Meetup in Moscow. "
        --             "Video (Russian): "
        --     a   ! href
        --             "http://ruhaskell.org/posts/talks/2015/06/21/\
        --             \haskell-for-pythonista.html"
        --         $   "ruhaskell.org/posts/talks/2015/06/21/\
        --             \haskell-for-pythonista.html"
        ]

    residence = do
        p $ toHtml moscow
        p "I'm able to relocate to Europe or North America."

    moscow = "Moscow, Russia"

    moscowChemicalLyceum = "The Moscow Chemical Lyceum (School 1303)"

    build target = do
        createDirectoryIfMissing True target
        ByteString.writeFile (target </> "cv.html") (renderCv cv)
        copyFile photo (target </> photo)
        putStrLn $ "built site in " <> show target
