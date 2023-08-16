{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CV.Render where

import Prelude hiding (div, head)

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
                 ContactInfo (Bitbucket, EMail, Facebook, GitHub, LinkedIn, Location, Personal, Skype, Telegram, Telephone, Twitter),
                 Education (Education), Month, Organization (At, Freelance),
                 Work (Work))
import CV.Types qualified

renderCv :: CV -> ByteString
renderCv
        CV
        { competencies
        , contactInfo
        , education
        , fullname
        , publicActivity
        , talks
        , technologies
        , workExperience
        } =
    renderHtml $
    docTypeHtml do
        head $ preamble fullname
        body do
            div ! class_ "container" $ do
                h1 $ toHtml fullname
                renderContacts contactInfo
                hr
                renderCompetencies competencies
                renderTechnologies technologies
                hr
                renderWorkExperience workExperience
                renderEducation education
                renderPublicActivity publicActivity
                renderTalks talks

preamble :: Text -> Html
preamble fullname = do
    meta ! charset "UTF-8"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    T.title $ toHtml fullname
    link
        ! href
            "https://cdn.jsdelivr.net\
            \/npm/bootstrap@5.2.0/dist/css/bootstrap.min.css"
        ! rel "stylesheet"
        ! integrity
            "sha384-\
            \gH2yIJqKdNHPEq0n4Mqa/HGKIhSkIHeL5AyhkYV8i59U5AR6csBvApHHNl/vI1Bx"
        ! crossorigin "anonymous"
    script  ! src
                "https://cdn.jsdelivr.net\
                \/npm/bootstrap@5.2.0/dist/js/bootstrap.bundle.min.js"
            ! integrity
                "sha384-\
                \A3rJD856KowSb7dwlZdYEkO39Gagi7vIsF0jrRAoQmDKKtQBHUuLZ9AsSv4jD4\
                \Xa"
            ! crossorigin "anonymous"
        $ ""
    T.style styles
  where
    crossorigin = customAttribute "crossorigin"
    integrity   = customAttribute "integrity"

renderContacts :: [ContactInfo] -> Html
renderContacts contactInfo =
    address $ mconcat $ intersperse br $ fmap contactMarkup contactInfo

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
    table ! class_ "table work" $
        for_ workExperience
            \Work
                { description
                , end = workEnd
                , location
                , organization
                , position
                , start
                , toolsAndTechs
                , totalTime
                , visible
                } ->
            when visible $
            tr do
                td ! class_ "col-2" $
                    p do
                        case workEnd of
                            Nothing -> "started " >> timeSpan start
                            Just end -> do
                                timeSpan start
                                preEscapedString "&nbsp;— "
                                timeSpan end
                        br
                        toHtml $ "(" <> totalTime <> ")"
                td ! class_ "col-10" $ do
                    p do
                        toHtml position
                        case organization of
                            At org -> do
                                " at "
                                T.span ! class_ "place" $ toHtml org
                            Freelance -> "freelance and short-time contracts"
                        ", "
                        toHtml location
                    description
                    unless (Text.null toolsAndTechs) $
                        p do
                            strong "Tools & Technologies:"
                            " "
                            toHtml toolsAndTechs

renderEducation :: [Education] -> Html
renderEducation education = do
    h3 "Education"
    table ! class_ "edu table" $
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
                td ! class_ "col-2" $
                    p $
                    T.span ! class_ "time" $
                        if graduated > 0 then
                            toHtml graduated
                        else
                            toHtml $ "(" <> show (negate graduated) <> ")"
                td ! class_ "col-8" $ do
                    p do
                        T.span ! class_ "place" $ toHtml school
                        unless (Text.null division) $
                            "," >> br >> toHtml division
                    description
                td ! class_ "col-2 degree" $ p $ toHtml degree

renderPublicActivity :: [((Int, Month), Html)] -> Html
renderPublicActivity publicActivity = do
    h3 "Public Activity"
    table ! class_ "achiev table" $
        for_ publicActivity \((year, month), description) ->
            tr do
                td ! class_ "col-2" $ p $ timeSpan (year, month)
                td ! class_ "col-10" $ description

renderTalks :: [((Int, Month), Html)] -> Html
renderTalks talks = do
    h4 "Conference talks"
    table ! class_ "achiev table" $
        for_ talks \((year, month), description) ->
            tr do
                td ! class_ "col-2" $ p $ timeSpan (year, month)
                td ! class_ "col-10" $ description

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
styles =
    toHtml
        [st|
            body                { font-size:        smaller;    }
            td, h1, h2, h3, li  { page-break-after: avoid;      }
            .time, .place       { font-weight:      bold;       }
            .time               { white-space:      nowrap;     }
        |]

contactMarkup :: ContactInfo -> Html
contactMarkup = \case
    Bitbucket user    -> ahref "https://bitbucket.org/" user
    EMail addr        -> ahref "mailto:" addr
    Facebook user     -> ahref "https://" $ "fb.me/" <> user
    GitHub user       -> ahref "https://" $ "github.com/" <> user
    LinkedIn short    -> ahref "https://" $ "linkedin.com/in/" <> short
    Location location -> toHtml location
    Personal url      -> ahref "https://" url
    Skype user        -> ahref "callto:" user
    Telegram user     -> ahref "https://t.me/" user
    Telephone number  -> toHtml number
    Twitter user      -> ahref "https://twitter.com/" user
  where
    ahref prefix url = a ! href (toValue (prefix <> url)) $ toHtml url
