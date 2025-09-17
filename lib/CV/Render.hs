{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
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

import CV.Types (
    CV (..),
    ContactInfo (..),
    Education (..),
    Month,
    Organization (..),
    Work (..),
 )

renderCv :: CV -> ByteString
renderCv cv =
    renderHtml . docTypeHtml $ do
        head $ preamble cv.fullname
        body do
            div ! class_ "container" $ do
                h1 do
                    toHtml cv.fullname
                    " "
                    T.span ! A.title (toValue cv.fullnameInfo) $ "ℹ️"
                renderContacts cv.contactInfo
                hr
                renderAbout cv.about
                renderTechnologies cv.technologies
                hr
                renderWorkExperience cv.workExperience
                -- renderEducations cv.education
                renderPublicActivity cv.publicActivity
                renderTalks cv.talks

preamble :: Text -> Html
preamble fullname = do
    meta ! charset "UTF-8"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    T.title $ toHtml fullname
    link
        ! href "https://cdn.jsdelivr.net/npm/bootstrap@5.2.0/dist/css/bootstrap.min.css"
        ! rel "stylesheet"
        ! integrity "sha384-gH2yIJqKdNHPEq0n4Mqa/HGKIhSkIHeL5AyhkYV8i59U5AR6csBvApHHNl/vI1Bx"
        ! crossorigin "anonymous"
    script
        ! src "https://cdn.jsdelivr.net/npm/bootstrap@5.2.0/dist/js/bootstrap.bundle.min.js"
        ! integrity "sha384-A3rJD856KowSb7dwlZdYEkO39Gagi7vIsF0jrRAoQmDKKtQBHUuLZ9AsSv4jD4Xa"
        ! crossorigin "anonymous"
        $ ""
    T.style styles
  where
    crossorigin = customAttribute "crossorigin"
    integrity = customAttribute "integrity"

renderContacts :: [ContactInfo] -> Html
renderContacts contactInfo =
    address $ mconcat $ intersperse br $ fmap contactMarkup contactInfo

renderAbout :: Html -> Html
renderAbout about = do
    h3 "About me"
    dl $ dd about

renderTechnologies :: [Html] -> Html
renderTechnologies technologies = do
    h4 "Key Technologies"
    dl $ dd $ toHtml $ intersperse ", " technologies

renderWorkExperience :: [Work] -> Html
renderWorkExperience workExperience = do
    h3 "Work Experience"
    table ! class_ "table work" $
        for_ workExperience $
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
                    when visible . tr $ do
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

renderEducations :: [Education] -> Html
renderEducations educations = do
    h3 "Education"
    table ! class_ "edu table" $
        for_ educations \edu -> when edu.visible $ rEducation edu
  where
    rEducation edu =
        tr do
            td ! class_ "col-2" $ rGraduated edu.graduated
            td ! class_ "col-8" $ rDescription edu
            td ! class_ "col-2 degree" $ rDegree edu.degree

    rGraduated g =
        p $
            T.span ! class_ "time" $
                if g > 0 then
                    toHtml g
                else
                    toHtml $ "(" <> show (negate g) <> ")"

    rDescription edu = do
        p do
            T.span ! class_ "place" $ toHtml edu.school
            unless (Text.null edu.division) $
                "," >> br >> toHtml edu.division
        edu.description

    rDegree degree = p $ toHtml degree

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
    Bitbucket user -> ahref "https://bitbucket.org/" user
    EMail addr -> ahref "mailto:" addr
    Facebook user -> ahref' $ "fb.me/" <> user
    GitHub user -> ahref' $ "github.com/" <> user
    LinkedIn short -> ahref' $ "linkedin.com/in/" <> short
    Location location -> toHtml location
    Personal url -> ahref' url
    Skype user -> ahref "callto:" user
    Telegram user -> ahref "https://t.me/" user
    Telephone number -> toHtml number
    Twitter user -> ahref "https://twitter.com/" user
  where
    ahref prefix url = a ! href (toValue (prefix <> url)) $ toHtml url
    ahref' = ahref "https://"
