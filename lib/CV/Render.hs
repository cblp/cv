{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CV.Render where

import Control.Monad (unless, when)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (for_)
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 as T
import Text.Blaze.Html5.Attributes as A hiding (start)
import Text.Shakespeare.Text (st)

import CV.Types (CV (CV),
                 ContactInfo (Bitbucket, EMail, Facebook, GitHub, LinkedIn, Personal, Skype, Telegram, Telephone, Twitter),
                 Education (Education), Month, Work (Work))
import CV.Types qualified

renderCv :: CV -> ByteString
renderCv
        CV
        { competencies
        , contactInfo
        , education
        , fullname
        , publicActivity
        , residence
        , talks
        , technologies
        , workExperience
        } =
    renderHtml $
    docTypeHtml do
        T.head $ preamble fullname
        body do
            renderContacts contactInfo
            h1 $ toHtml fullname
            hr
            renderCompetencies competencies
            renderTechnologies technologies
            renderWorkExperience workExperience
            renderEducation education
            renderPublicActivity publicActivity
            renderTalks talks
            renderResidence residence

preamble :: Text -> Html
preamble fullname = do
    meta ! charset "UTF-8"
    T.title $ toHtml fullname
    T.style styles

renderContacts :: [ContactInfo] -> Html
renderContacts contactInfo =
    T.div ! class_ "contact-info" $
    table do
        tr $
            td2 do
                h3 "Contact Info"
                p "Time zone is CET (UTC+2)"
        for_ contactInfo \contact -> do
            let (contactLabel, contactContent) = contactMarkup contact
            tr do
                td $ toHtml contactLabel
                td contactContent
  where
    td2 = td ! colspan (toValue (2 :: Int))

renderCompetencies :: Html -> Html
renderCompetencies competencies = do
    h3 "Competencies"
    dl $ dd competencies

renderTechnologies :: [Html] -> Html
renderTechnologies technologies = do
    h4 "Key Technologies"
    dl $ dd $ toHtml $ intersperse ", " technologies

renderWorkExperience :: [Work] -> Html
renderWorkExperience workExperience = do
    h3 "Work Experience"
    table ! class_ "work" $
        for_ workExperience
            \Work
                { description
                , end = workEnd
                , location
                , organization
                , position
                , start
                , totalTime
                , visible
                } ->
            when visible $
            tr do
                td $ p do
                    case workEnd of
                        Nothing -> "started " >> timeSpan start
                        Just end -> do
                            timeSpan start
                            preEscapedString "&nbsp;— "
                            timeSpan end
                    br
                    toHtml $ "(" <> totalTime <> ")"
                td do
                    p do
                        toHtml position
                        br
                        "at "
                        T.span ! class_ "place" $ toHtml organization
                        ", "
                        toHtml location
                    description

renderEducation :: [Education] -> Html
renderEducation education = do
    h3 "Education"
    table ! class_ "edu" $
        for_ education
            \Education
                { degree
                , description
                , division
                , graduated
                , school
                , visible
                } ->
            when visible $
            tr do
                td $ p $ T.span ! class_ "time" $
                    if graduated > 0 then
                        toHtml graduated
                    else
                        toHtml $ "(" <> show (negate graduated) <> ")"
                td do
                    p do
                        T.span ! class_ "place" $ toHtml school
                        unless (Text.null division) $
                            "," >> br >> toHtml division
                    description
                td ! class_ "degree" $ p $ toHtml degree

renderPublicActivity :: [((Int, Month), Html)] -> Html
renderPublicActivity publicActivity = do
    h3 "Public Activity"
    table ! class_ "achiev" $
        for_ publicActivity \((year, month), description) ->
            tr do
                td . p $ timeSpan (year, month)
                td description

renderTalks :: [((Int, Month), Html)] -> Html
renderTalks talks = do
    h4 "Conference talks"
    table ! class_ "achiev" $
        for_ talks \((year, month), description) ->
            tr do
                td $ p $ timeSpan (year, month)
                td description

renderResidence :: Html -> Html
renderResidence residence = do
    h3 "Residence"
    dl $ dd residence

timeSpan :: (Int, Month) -> Html
timeSpan (year, month) =
    T.span ! class_ "time" $ do
        toHtml $ show month
        " "
        toHtml year

-- nobr = T.span ! A.style "white-space: nowrap;"

styles :: Html
styles = toHtml [st|
    a {
        color: blue;
    }

    abbr {
        border-bottom: 1px dotted;
    }

    body {
        font-family: Georgia, serif;
        padding: 3em;
    }

    h2 {
        font-weight: normal;
    }

    h3 {
        margin-top: 1em;
        margin-bottom: 0.5em;
    }

    sup {
        font-size: small;
        vertical-align: 1.2em;
    }

    .edu, .work, .achiev {
        padding-left: 0.7em;
        /* border-spacing: 1em; */
    }

    td {
        vertical-align: top;
    }

    td, h1, h2, h3, li {
        page-break-after: avoid;
    }

    .edu td, .work td, .achiev td {
        padding-left: 1em;
    }

    .edu td.degree {
        /* font-style: italic; */
    }

    .time, .place {
        font-weight: bold;
    }

    .time {
        white-space: nowrap;
    }

    .contact-info {
        float: right;
        margin-left: 1em;
        padding-left: 1em;
        border-left: dashed 1px gray;
        width: 33%;
    }

    ul { margin-top: 0; }

    .li-number {
        text-align: right;
    }

    p {
        margin-top: 0;
    }

    hr {
        border-bottom: none;
        border-top: solid 1px black;
        height: 0;
    }
|]

contactMarkup :: ContactInfo -> (Text, Html)
contactMarkup = \case
    Bitbucket user -> ("Bitbucket", ahref "https://bitbucket.org/" user)
    EMail addr -> ("E-mail", ahref "mailto:" addr)
    Facebook user -> ("Facebook", ahref "https://" ("fb.me/" <> user))
    GitHub user -> ("GitHub", ahref "https://github.com/" user)
    LinkedIn short ->
        ("LinkedIn", ahref "https://" ("linkedin.com/in/" <> short))
    Personal url -> ("Personal", ahref "https://" url)
    Skype user -> ("Skype", ahref "callto:" user)
    Telegram user -> ("Telegram", ahref "https://telegram.me/" user)
    Telephone number -> ("Tel.", toHtml number)
    Twitter user -> ("Twitter", ahref "https://twitter.com/" user)
  where
    ahref prefix url = a ! href (toValue (prefix <> url)) $ toHtml url
