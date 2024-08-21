import CV.Types

import Data.ByteString.Lazy as BS (writeFile)
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Tuple.X ((-:))
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Text.Blaze.Html5 (a, p, (!))
import Text.Blaze.Html5.Attributes (href)

import CV.Render (renderCv)
import GitHubPages (deploy)

cv :: CV
cv =
    CV
        { fullname = "Yuriy Syrovetskiy"
        , contactInfo =
            [ Location "Montenegro (UTC+1), remote"
            , EMail "job38@cblp.su"
            , LinkedIn "cblpsu"
            , GitHub "cblp"
            -- , Personal "cblp.github.io"
            -- , Telephone "+7 905 547 11 98"
            -- , Skype "cblp.su"
            -- , Telegram "cblp_su"
            -- , Bitbucket "cblp"
            -- , Facebook "cblp.su"
            -- , Twitter "cblp_su"
            ]
        , competencies = do
            p
                "Ability to work in a complex environment combining different\
                \ technologies."
            p $
                fold $
                    intersperse
                        ", "
                        [ "Distributed systems (CRDT)"
                        , "Functional programming"
                        , "Web backend"
                        , "Compilers"
                        , "Data analysis"
                        , "Scalable services"
                        , "Security"
                        , "User interface"
                        ]
            p $
                fold $
                    intersperse
                        ", "
                        [ "Software design"
                        , "Coding"
                        , "Deployment"
                        , "Staff training"
                        ]
        , technologies = ["C++", "Haskell", "Java", "Python"]
        , workExperience =
            [ Work
                { start = (2023, Oct)
                , end = Nothing
                , totalTime = "10 months"
                , organization = At "SQream"
                , location = "Montenegro/remote"
                , position = "Senior Software Engineer"
                , description =
                    p
                        "Working as an engineer in support of the SQream GPU-powered\
                        \ database engine.\
                        \ Fixing various bugs in C++ engine,\
                        \ Haskell query compiler and CUDA kernels."
                , toolsAndTechs = "C++, Haskell, CUDA (GPU)"
                , visible = True
                }
            , Work
                { start = (2022, Jun)
                , end = Just (2023, Oct)
                , totalTime = "1⅓ year"
                , organization = Freelance
                , location = "remote"
                , position = ""
                , description = do
                    p
                        "Lead developer of internal system of issue tracking and decision\
                        \ making for a large DAO.\
                        \ Web backend + frontend.\
                        \ Stellar blockchain integration for automatic payment processing\
                        \ and access management.\
                        \ Telegram integration for control via chat.\
                        \ Tooling to mint and trade NFT representing real estate ownership."
                    p "Various Python and Haskell projects."
                    p
                        "Short-term contract with Generation Lambda.\
                        \ Built an automated NFT distribution service on Cardano network."
                , -- p "Short-term contract with Yandex. ???"
                  toolsAndTechs =
                    "C++, ClickHouse, Docker, Ed25519 cryptography\
                    \, Haskell (Servant, Yesod), HTTP, Java, JSON, Python\
                    \, Scala, Sqlite, Stellar blockchain, Telegram API, YTsaurus"
                , visible = True
                }
            , Work
                { start = (2023, Apr)
                , end = Nothing
                , totalTime = "3 months"
                , organization = yandex
                , location = "remote"
                , position = "Senior Software Engineer"
                , description =
                    "Developer of the massive search engine events and expenditure\
                    \ real-time accounting system."
                , toolsAndTechs = "C++, ClickHouse, Python, YTsaurus"
                , visible = False
                }
            , Work
                { start = (2022, Jun)
                , end = Just (2022, Aug)
                , totalTime = "3 months"
                , organization = At "Generation Lambda"
                , location = "remote"
                , position = "Senior Software Engineer"
                , description =
                    p
                        "Developing Maladex — a distributed exchange based on the Cardano\
                        \ smart contracts.\
                        \ Created an NFT distribution testing service.\
                        \ Designed internal infrastructure tools."
                , toolsAndTechs = "Cardano blockchain (NFT, tokens), Haskell, JSON"
                , visible = False
                }
            , Work
                { start = (2021, Feb)
                , end = Just (2022, May)
                , totalTime = "1⅓ year"
                , organization = At "Input-Output Global"
                , location = "remote"
                , position = "Software Engineer"
                , description =
                    p
                        "Cardano blockchain node development. \
                        \Implemented some features for the Alonzo era."
                , toolsAndTechs =
                    "Cardano blockchain (API, CLI, node), CBOR, Haskell, JSON"
                , visible = True
                }
            , Work
                { start = (2020, Sep)
                , end = Just (2021, Mar)
                , totalTime = "7 months"
                , organization = At "Higher School of Economics"
                , location = moscow
                , position = "Haskell teacher"
                , description =
                    p "Teaching programming in Haskell to 1—4th year students."
                , toolsAndTechs = ""
                , visible = True
                }
            , Work
                { start = (2020, Mar)
                , end = Just (2020, Dec)
                , totalTime = "9 months"
                , organization = yandex
                , location = moscow
                , position = "Software Engineer"
                , description = do
                    p "Writing Java in Yandex.Disk cloud core."
                    p "Implemented cross-service file metadata synchronization machinery."
                    p
                        "Working on a CRDT-based application state synchronization\
                        \ framework."
                , toolsAndTechs = "ClickHouse, HTTP, Java, JSON, PostgreSQL, Python"
                , visible = True
                }
            , Work
                { start = (2016, Feb)
                , end = Just (2020, Mar)
                , totalTime = "4 years"
                , organization = At "Kaspersky Lab"
                , location = moscow
                , position = "Software Engineer, then Senior Software Engineer"
                , description = p do
                    "As a Kaspersky OS and Kaspersky Security System "
                    "development team member, "
                    "I design DSLs, implement security configuration compiler "
                    "and various security policies "
                    "(including object-capability model), "
                    "using Haskell for high-level logic and generating code in C."
                , -- TODO achievements in numbers?
                  toolsAndTechs =
                    "C (kernel level, hard realtime, lock-free data)\
                    \, Haskell (Ivory, Megaparsec)"
                , visible = True
                }
            , Work
                { start = (2012, Sep)
                , end = Just (2018, May)
                , totalTime = "6 years"
                , organization = At moscowChemicalLyceum
                , location = moscow
                , position = "Student scientific projects mentor"
                , description = do
                    p do
                        "Some students presented their work at school science "
                        "project fairs. "
                    p do
                        "In 2014, Polina Kirichenko won 1st prize with her work on "
                        "natural language-based programming at the Yandex "
                        "conference on programming for school students. "
                        a
                            ! href
                                "https://academy.yandex.ru/events/school-conf/\
                                \msk-apr-2014/#winners"
                            $ "academy.yandex.ru"
                    p do
                        "In 2017, Nikolay Loginov presented his work on CRDT at "
                        "the industrial conference on functional programming "
                        "FPCONF (co-presented with me). "
                        a ! href "http://fpconf.ru/2017.html" $ "fpconf.ru"
                , toolsAndTechs = ""
                , visible = False
                }
            , Work
                { start = (2011, Dec)
                , end = Just (2016, Feb)
                , totalTime = "4 years"
                , organization = yandex
                , location = moscow
                , position = "Software Engineer"
                , description = do
                    p do
                        "I was a backend developer of the keyword statistics "
                        "service "
                        a ! href "http://wordstat.yandex.com/" $
                            "Wordstat.yandex.com"
                        " and several internal Yandex services."
                    p do
                        "Wordstat is a search-oriented database "
                        "capable of scanning over 1 billion records at a user request."
                    p "I worked with computer linguistics."
                    p "I developed microservices using HTTP JSON-API."
                    p do
                        "My software successfully stands year-to-year growing "
                        "data and user traffic."
                    p do
                        "One microservice rewritten by me from Python to C++, "
                        "got about 10 times boost in maximum request load. "
                    p do
                        "Another one got 1.5 times decrease in memory consumption "
                        "without a visible performance penalty after introducing "
                        "internal compression."
                    p do
                        "I gave talks at the company's local meetups "
                        "(internal and public) "
                        a ! href "https://events.yandex.ru/events/meetings/29-october-2015/" $
                            "events.yandex.ru"
                , toolsAndTechs =
                    "Apache Thrift, BerkeleyDB, BitTorrent, C++, Cap'n'proto, Cython\
                    \, Map/Reduce, PostgreSQL, Python, Yandex.Tank"
                , visible = True
                }
            , Work
                { start = (2015, Jan)
                , end = Just (2015, Jun)
                , totalTime = "1 semester"
                , organization = At moscowChemicalLyceum
                , location = moscow
                , position = "Teacher of functional programming (Haskell)"
                , description =
                    p "Optional subject for 9th, 10th and 11th grade students."
                , toolsAndTechs = ""
                , visible = False
                }
            , Work
                { start = (2006, Nov)
                , end = Just (2011, Oct)
                , totalTime = "5 years"
                , organization = At "Institute of Information Technologies"
                , location = moscow
                , position = "Engineer, then Team Lead & Architect"
                , description = do
                    p do
                        "I was the lead developer in 3-people team working on "
                        "multi-component data transfer and processing system."
                    p do
                        "I've been working on design and code, program and "
                        "user documentation, deploy and customer support."
                    p do
                        "During the development process, "
                        "I designed and implemented software components using "
                        "C, C++ and Python languages, and Qt GUI framework. "
                        "I used static and dynamic program analysis tools "
                        "to detect and fix bugs such as memory leaks."
                    p do
                        "In my department (~20 people), "
                        "I introduced usage of source control tools, "
                        "issue management, common knowledge system (wiki)."
                , toolsAndTechs = "C, C++, Linux kernel, Python, Qt, Windows API, XML"
                , visible = True
                }
            ]
        , education =
            [ Education
                { graduated = 2020
                , school = "The Moscow Aviation Institute"
                , division =
                    "faculty of control systems and computer science in engineering"
                , degree = "Master"
                , description = p do
                    "Master's thesis: A distributed embedded database. "
                    "I've built a CRDT-based database-like application framework for "
                    "data synchronization and a distributed project management software "
                    "on top of the framework. "
                    "Both in Haskell. "
                    "The source code is available at "
                    a ! href "https://github.com/ff-notes" $ "github.com/ff-notes"
                    "."
                , visible = True
                }
            , Education
                { graduated = 2010
                , school = "The Moscow Institute of Humanities and Economics"
                , division = "faculty of law"
                , degree = "higher/specialist in jurisprudence, civil law"
                , description = ""
                , visible = False
                }
            , Education
                { graduated = 2006
                , school =
                    "Institute of Cryptography, Communications and Informatics"
                , division = "faculty of information security"
                , degree = "incomplete higher in computer security"
                , description = ""
                , visible = False
                }
            , Education
                { graduated = 2002
                , school = moscowChemicalLyceum
                , division = "faculty of physics and mathematics"
                , degree = "secondary"
                , description = ""
                , visible = False
                }
            ]
        , publicActivity =
            let
                coLaboratoryRuhaskell = do
                    "Co-organized RuHaskell community meetup "
                    "in Kaspersky, Moscow, Russia. "
                    "4 talks, 120+ attendees. "
                ruhaskellExtropolis = do
                    "Organized RuHaskell community meetup in Moscow, Russia. "
                    "6 talks, 50+ attendees. "
             in
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
                    a
                        ! href
                            "http://ruhaskell.org/posts/events/2015/11/05/\
                            \meetup-winter-register.html"
                        $ "ruhaskell.org/posts/events/2015/11/05/\
                          \meetup-winter-register.html"
                , (2015, Jun) -: p do
                    ruhaskellExtropolis
                    a
                        ! href
                            "https://github.com\
                            \/ruHaskell/ruhaskell/wiki/Meetup.2015.Summer"
                        $ "github.com\
                          \/ruHaskell/ruhaskell/wiki/Meetup.2015.Summer"
                ]
        , talks =
            [ (2020, Jul) -: p do
                "“Property and fuzzy testing” at C++ Russia. "
                a ! href "https://cppconf-moscow.ru" $ "cppconf-moscow.ru"
            , (2019, May) -: p do
                "“A purely functional approach to CRDT/RON-based "
                "distributed systems” at FPURE. "
                a ! href "https://www.fpure.events" $ "fpure.events"
                ", video: "
                a ! href "https://youtu.be/2MKLWCh33wE" $ "youtu.be/2MKLWCh33wE"
            , -- , (2019, Mar) -: do
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
              (2018, Sep) -: p do
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
        }
  where
    moscow = "Moscow, Russia"

    moscowChemicalLyceum = "The Moscow Chemical Lyceum (School 1303)"

    yandex = At "Yandex"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-d"] -> deploy build
        [] -> build "_site"
        _ -> error "don't know what to do"
  where
    build target = do
        createDirectoryIfMissing True target
        BS.writeFile (target </> "index.html") (renderCv cv)
        putStrLn $ "built site in " <> show target
