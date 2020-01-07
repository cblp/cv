{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module CV.Render where

import           Control.Monad (unless, when)
import           Data.ByteString.Lazy (ByteString)
import           Data.Foldable (for_)
import           Data.List (intersperse)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Blaze.Html5 as T
import           Text.Blaze.Html5.Attributes as A hiding (start)
import           Text.Shakespeare.Text (st)

import           CV.Types (CV (..), ContactInfo (..), Education (..), Work (..))

renderCv :: CV -> ByteString
renderCv CV{..} =
    renderHtml . docTypeHtml $ do
        T.head $ do
            meta ! charset "UTF-8"
            T.title $ toHtml fullname
            T.style styles
        body $ do
            T.div ! class_ "contact-info" $
                table $
                -- tr $
                --     td ! colspan (toValue (2 :: Int)) $
                --     img ! src (toValue photo)
                 do
                    tr $ td ! colspan (toValue (2 :: Int)) $ h3 "Contact Info"
                    for_ contactInfo $
                        \(contactMarkup -> (contactLabel, contactContent)) ->
                            tr $ do
                                td $ toHtml contactLabel
                                td contactContent
            h1 $ toHtml fullname
            hr !
                A.style
                    "height: 0; \
                    \border-top: solid 1px black; \
                    \border-bottom: none;"
            h3 "Competencies"
            dl $ dd competencies
            h4 "Key Technologies"
            dl $ dd $ toHtml $ intersperse ", " technologies
            h3 "Work Experience"
            table ! class_ "work" $
                for_ workExperience $ \Work{end = workEnd, ..} ->
                    when visible $ tr $ do
                        td $ p $ do
                            case workEnd of
                                Nothing ->
                                    "started " >> timeSpan start
                                Just end -> do
                                    timeSpan start
                                    preEscapedString "&nbsp;— "
                                    timeSpan end
                            br
                            toHtml $ "(" <> totalTime <> ")"
                        td $ do
                            p $ do
                                toHtml position
                                br
                                "at "
                                T.span ! class_ "place" $ toHtml organization
                                ", "
                                toHtml location
                            description
            h3 "Education"
            table ! class_ "edu" $
                for_ education $ \Education{..} ->
                    when visible $ tr $ do
                        td $ p $ T.span ! class_ "time" $
                            if graduated > 0 then
                                toHtml graduated
                            else
                                toHtml $ "(" <> show (negate graduated) <> ")"
                        td $ do
                            p $ do
                                T.span ! class_ "place" $ toHtml school
                                unless (Text.null division) $ do
                                    ","
                                    br
                                    toHtml division
                            description
                        td ! class_ "degree" $ p $ toHtml degree
            h3 "Public Activity"
            table ! class_ "achiev" $
                for_ publicActivity $ \((year, month), description) ->
                    tr $ do
                        td . p $ timeSpan (year, month)
                        td description
            h4 "Conference talks"
            table ! class_ "achiev" $
                for_ talks $ \((year, month), description) ->
                    tr $ do
                        td . p $ timeSpan (year, month)
                        td description
            h3 "Residence"
            dl $ dd residence
  where
    timeSpan (year, month) =
        T.span ! class_ "time" $ do
            toHtml $ show month
            " "
            toHtml year
    -- nobr = T.span ! A.style "white-space: nowrap;"
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
