{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString.Lazy as ByteString (writeFile)
import           Data.Foldable (fold)
import           Data.List (intersperse)
import           Data.Tuple.X ((-:))
import           System.Directory (createDirectoryIfMissing)
import           System.Environment (getArgs)
import           System.FilePath ((</>))
import           Text.Blaze.Html5 (a, p, toHtml, (!))
import           Text.Blaze.Html5.Attributes (href)

import           CV.Render (renderCv)
import           CV.Types (CV (..), ContactInfo (..), Education (..),
                           Month (..), Work (..))
import           GitHubPages (deploy)

cv :: CV
cv = CV
  { fullname = "Yuriy Syrovetskiy"

  , contactInfo = [
      Telephone "+7 905 547 11 98",
      Skype "cblp.su",
      EMail "cblp@cblp.su",
      Telegram "cblp_su",
      Personal "cblp.github.io",
      GitHub "cblp",
      Bitbucket "cblp",
      LinkedIn "cblpsu",
      Facebook "cblp.su",
      Twitter "cblp_su"
      ]


  , competencies = do
      p $
        fold $
        intersperse
          ", "
          [ "Distributed systems (CRDT)", "Functional programming",
            "Web backend", "Compilers", "Data analysis", "Scalable services",
            "Security", "User interface"
          ]
      p "Design, Coding, Project management, Deployment, Staff training"

  , technologies = ["C", "C++", "Haskell", "Linux", "Python"]

  , workExperience = [
      Work{
        start = (2016, Feb),
        end = Nothing,
        totalTime = "4 years",
        organization = "Kaspersky Lab",
        location = moscow,
        position = "Senior Software Engineer",
        description = p $ do
          "As a Kaspersky OS and Kaspersky Security System "
          "development team member, "
          "I design DSLs, implement security configuration compiler "
          "and various security policies "
          "(including object-capability model), "
          "using Haskell for high-level logic and generating code in C.",
          -- TODO achiements in numbers?
        visible = True
        },
      Work{
        start = (2012, Sep),
        end = Nothing,
        totalTime = "7 years",
        organization = moscowChemicalLyceum,
        location = moscow,
        position = "Student scientific projects mentor",
        description = do
          p $ do
            "Some students presented their work at school science "
            "project fairs. "
          p $ do
            "In 2014, Polina Kirichenko won 1st prize with her work on "
            "natural language-based programming at the Yandex "
            "conference on programming for school students. "
            a ! href
                  "https://academy.yandex.ru/events/school-conf/\
                  \msk-apr-2014/#winners"
              $ "academy.yandex.ru"
          p $ do
            "In 2017, Nikolay Loginov presented his work on CRDT at "
            "the industrial conference on functional programming "
            "FPCONF (co-presented with me). "
            a ! href "http://fpconf.ru/2017.html" $ "fpconf.ru",
        visible = True
        },
      Work{
        start = (2011, Dec),
        end = Just (2016, Feb),
        totalTime = "4 years",
        organization = "Yandex",
        location = moscow,
        position = "Software Engineer",
        description = do
          p $ do
            "I was a backend developer of the keyword statistics "
            "service "
            a ! href "http://wordstat.yandex.com/" $
              "Wordstat.yandex.com"
            " and several internal Yandex services."
          p $ do
            "I worked with computer linguistics. "
            "I developed problem-specific databases in microservice "
            "architecture. "
          p $ do
            "My software successfully stands year-to-year growing "
            "data and user traffic."
          p $ do
            "One microservice rewritten by me from Python to C++, "
            "got about 10 times boost in maximum request load. "
          p $ do
            "Another one got 1.5 times decrease in memory consumption "
            "without a visible performance penalty after introducing "
            "internal compression."
          p $ do
            "I gave talks at the company's local meetups "
            "(internal and public) "
            a ! href "https://events.yandex.ru/events/meetings/29-october-2015/"
              $ "events.yandex.ru",
        visible = True
        },
      Work{
        start = (2015, Jan),
        end = Just (2015, Jun),
        totalTime = "1 semester",
        organization = moscowChemicalLyceum,
        location = moscow,
        position = "Teacher of functional programming (Haskell)",
        description =
          p "Optional subject for 9th, 10th and 11th grade students.",
        visible = False
        },
      Work{
        start = (2006, Nov),
        end = Just (2011, Oct),
        totalTime = "5 years",
        organization = "Research Institute of Information Technologies",
        location = moscow,
        position = "Engineer",
        description = do
          p $ do
            "I was the lead developer in 3-people team working on "
            "multi-component data transfer and processing system."
          p $ do
            "I've been working on design and code, program and "
            "user documentation, deploy and customer support."
          p $ do
            "In my department (~20 people), "
            "I introduced usage of source control tools, "
            "issue management, common knowledge system (wiki).",
        visible = True
        }
      ]


  , education = [
      Education{
        graduated = 2020,
        school = "The Moscow Aviation Institute",
        division =
          "faculty of control systems and computer science in engineering",
        degree = "Master",
        description = p $ do
          "Master's thesis: A distributed embedded database. "
          "I've built a CRDT-based database-like application framework for "
          "data synchronization and a distributed project management software "
          "on top of the framework. "
          "Both in Haskell. "
          "The source code is available at "
          a ! href "https://github.com/ff-notes" $ "github.com/ff-notes"
          ".",
        visible = True
        },
      Education{
        graduated = 2010,
        school = "The Moscow Institute of Humanities and Economics",
        division = "faculty of law",
        degree = "higher/specialist in jurisprudence, civil law",
        description = "",
        visible = False
        },
      Education{
        graduated = 2006,
        school =
          "Institute of Cryptography, Communications and Informatics",
        division = "faculty of information security",
        degree = "incomplete higher in computer security",
        description = "",
        visible = True
        },
      Education{
        graduated = 2002,
        school = moscowChemicalLyceum,
        division = "faculty of physics and mathematics",
        degree = "secondary",
        description = "",
        visible = False
        }
      ]


  , publicActivity = let
      coLaboratoryRuhaskell = do
        "Co-organized RuHaskell community meetup "
        "in Kaspersky, Moscow, Russia. "
        "4 talks, 120+ attendees. "
      ruhaskellExtropolis = do
        "Organized RuHaskell community meetup in Moscow, Russia. "
        "6 talks, 50+ attendees. "
      in [
        (2017, Apr) -: p $ do
          coLaboratoryRuhaskell
          a ! href "https://events.kaspersky.com/event/ruhaskell2" $
            "events.kaspersky.com/event/ruhaskell2",
        (2016, Aug) -: p $ do
          coLaboratoryRuhaskell
          a ! href "https://events.kaspersky.com/event/ruhaskell" $
            "events.kaspersky.com/event/ruhaskell",
        (2015, Dec) -: p $ do
          ruhaskellExtropolis
          a ! href
                "http://ruhaskell.org/posts/events/2015/11/05/\
                \meetup-winter-register.html"
            $ "ruhaskell.org/posts/events/2015/11/05/\
              \meetup-winter-register.html",
        (2015, Jun) -: p $ do
          ruhaskellExtropolis
          a ! href
                "https://github.com/ruHaskell/ruhaskell/wiki/Meetup.2015.Summer"
            $ "github.com/ruHaskell/ruhaskell/wiki/Meetup.2015.Summer"
        ]


  , talks = [
      (2019, May) -: p $ do
        "“A purely functional approach to CRDT/RON-based "
        "distributed systems” at FPURE. "
        a ! href "https://www.fpure.events" $ "fpure.events"
        ", video: "
        a ! href "https://youtu.be/2MKLWCh33wE" $ "youtu.be/2MKLWCh33wE",
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
      (2018, Sep) -: p $ do
        "“Purely functional programming and KasperskyOS” "
        "in Information Security section at RIFTECH. "
        a ! href "http://tech.rif.ru" $ "tech.rif.ru",
      (2017, Dec) -: p $ do
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


  , residence = do
      p $ toHtml moscow
      p "I'm open to relocation." -- to Europe or North America."
  }

  where
    moscow = "Moscow, Russia"

    moscowChemicalLyceum = "The Moscow Chemical Lyceum (School 1303)"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-d"] -> deploy build
    []     -> build "_site"
    _      -> error "don't know what to do"
  where
    build target = do
      createDirectoryIfMissing True target
      ByteString.writeFile (target </> "index.html") (renderCv cv)
      putStrLn $ "built site in " <> show target
