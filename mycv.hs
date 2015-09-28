import CV

name = T{en="Yuriy Syrovetskiy", ru="Юрий Сыровецкий"}

photo = "Yuriy_Syrovetskiy.jpg"

contactInfo = [ Telephone     "+7 905 547 11 98"
              , Skype         "cblp.su"
              , EMail         "cblp@cblp.su"
              -- , Jabber        "cblp@cblp.su"
              , Telegram      "cblp_su"
              , PersonalPage  "cblp.su"
              , LinkedIn      "cblpsu"
              , Facebook      "cblp.su"
              , GitHub        "cblp"
              , Bitbucket     "cblp"
              , Twitter       "cblp_su"
              ]

professionalSkills =
    [ "Desktop and server (backend) programming. Data analysis, high load services, user interface design."
    , "Coding, project management, deployment, staff training."
    ]

technologies = Technologies
    { goodAt  = [ "C", "C++", "English", "git", "Haskell"
                , "Linux [Debian, Ubuntu]", "Mercurial", "Python", "Qt"
                , "Russian", "Subversion" ]
    , canUse  = [ "Boost", "HTML", "JavaScript", "Java", "Perl", "PHP"
                , "Windows", "XML" ]
    , canRead = [ "Assembler", "Erlang", ".NET/C#", "LISP/Clojure/Scheme"
                , "Ruby", "Scala", "Smalltalk", "other cool stuff" ]
    }

workExperience =
    [ Work  { start = (2015, Jan), end = Nothing, totalTime = "1 semester"
            , organization = "The Moscow Chemical Lyceum"
            , location = "Moscow, Russia"
            , title = "teacher of functional programming (Haskell)"
            }
    , Work  { start = (2012, Sep), end = Nothing, totalTime = "3 years"
            , organization = "The Moscow Chemical Lyceum"
            , location = "Moscow, Russia"
            , title = "mentor in science works"
            }
    , Work  { start = (2011, Dec), end = Nothing, totalTime = "3½ years"
            , organization = "Yandex"
            , location = "Moscow, Russia"
            , title = "software developer"
            , description =
                  [ "I'm a backend developer of the keyword statistics service"
                    <> " <a href=\"http://wordstat.yandex.com/\">Wordstat.yandex.com</a>"
                    <> " and several internal Yandex services."
                  , "My software successfully stands year-to-year growing data and user traffic."
                  ]
            }
    , Work  { start = (2006, Nov), end = Just (2011, Oct), totalTime = "5 years"
            , organization = "Research Institute of Information Technologies"
            , location = "Moscow, Russia"
            , title = "engineer"
            , description =
                  [ "I was the lead developer of multi-component software system."
                  , "I've been working on design and code, program and user documentation, deploy and customer support."
                  , "In my team, I introduced usage of source control tools, issue management, common knowledge system (wiki)."
                  ]
            }
    ]

education =
    [ Education { graduated = 2010
                , school = "The Moscow Institute of Humanities and Economics"
                , division = "faculty of law"
                , degreee = "higher/specialist in jurisprudence, civil law"
                }
    , Education { graduated = 2006
                , school = "Institute of Cryptography, Communications and Informatics"
                , division = "faculty of information security"
                , degreee = "incomplete higher in computer security"
                }
    , Education { graduated = 2002
                , school = "The Moscow Chemical Lyceum"
                , division = "faculty of physics and mathematics"
                , degreee = "secondary"
                }
    ]

achievements =
    [ ( 2015, Jun
      , [ "Organized Haskell meetup/conference in Moscow, Russia: 6 talks, 50+ attendees"
          <> " (schedule in Russian:"
          <> " <a href=\"https://github.com/ruHaskell/ruhaskell/wiki/Meetup2015Summer\">"
          <> "github.com/ruHaskell/ruhaskell/wiki/Meetup2015Summer</a>)."
        ]
      )
    ]

residence = [ "Moscow, Russia."
            , "Ready to relocate."
            ]
