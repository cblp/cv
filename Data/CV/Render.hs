{-# LANGUAGE  LambdaCase
            , OverloadedStrings
            , QuasiQuotes
            , RecordWildCards
            , ViewPatterns
  #-}

module Data.CV.Render where

import Control.Monad
import Data.ByteString.Lazy
import Data.CV.Types
import Data.List as List
import Data.Monoid
import System.FilePath
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html5 as T -- tags
import Text.Blaze.Html5.Attributes as A -- attributes
import Text.Shakespeare.Text

renderCv :: Locale -> CV -> ByteString
renderCv locale CV{..} = renderHtml . docTypeHtml $ do
    T.head $ do
        meta ! charset "UTF-8"
        T.title . toHtml $ local fullname
        T.style styles
    body $ do
        T.div ! class_ "contact-info" $
            table $ do
                tr $ td ! colspan (toValue (2 :: Int)) $
                    img ! src (toValue photo)
                tr $ td ! colspan (toValue (2 :: Int)) $
                    h3 $ localized $ \case  En -> "Contact Info"
                                            Ru -> "Контакты"
                forM_ contactInfo $ \(contactMarkup -> (contactLabel, contactContent)) ->
                    tr $ do
                        td $ toHtml $ local contactLabel
                        td contactContent

        h1 . toHtml $ local fullname
        hr ! A.style "height: 0; border-top: solid 1px black; border-bottom: none;"

        h3 $ localized $ \case  En -> "Professional Skills"
                                Ru -> "Навыки и умения"
        local professionalSkills

        h4 $ localized $ \case  En -> "Technologies and Languages"
                                Ru -> "Технологии и языки"
        ul $ forM_ technologies $ \(techGroup, tech) ->
            li $ do
                em . toHtml $ local techGroup
                void " "
                toHtml $ List.intercalate ", " (List.map local tech) <> "."

        h3 $ localized $ \case  En -> "Work Experience"
                                Ru -> "Опыт работы"
        table ! class_ "work" $ forM_ workExperience $ \Work{..} -> tr $ do
            td $ do
                localized $ \case
                    En -> case workEnd of
                        Nothing -> do
                            void "started "
                            timeSpan workStart
                        Just end -> do
                            timeSpan workStart
                            void " — "
                            timeSpan end
                    Ru -> do
                        timeSpan workStart
                        void " — "
                        case workEnd of
                            Nothing ->
                                abbr ! A.title "настоящее время" $ nobr "н. в."
                            Just end ->
                                timeSpan end
                br
                toHtml $ "(" <> local totalTime <> ")"
            td $ do
                toHtml position
                br
                void $ localized $ \case  En -> "at "
                                          Ru -> ""
                T.span ! class_ "place" $ toHtml $ local organization
                void ", "
                toHtml $ local location
                description

        h3 "Education"
        table ! class_ "edu" $ forM_ education $ \Education{..} -> tr $ do
            td $ do
                void "grad. "
                T.span ! class_ "time" $ toHtml graduated
            td $ do
                T.span ! class_ "place" $ toHtml $ local school
                void ","
                br
                toHtml division
            td ! class_ "degree" $
                toHtml degree

        h3 "Achievements"
        table ! class_ "achiev" $ forM_ achievements $ \(year, month, description) -> tr $ do
            td $ timeSpan (year, month)
            td description

        h3 "Residence"
        dl . dd $
            residence
  where
    local (Localized f) = f locale
    localized f = f locale

    timeSpan (year, month) =
        T.span ! class_ "time" $ do
            toHtml $ localized $ \case En -> show month; Ru -> showRu month
            void " "
            toHtml year

    nobr = T.span ! A.style "white-space: nowrap;"

    styles :: Html
    styles = toHtml [st|
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
            border-spacing: 1em;
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
        }

        ul { margin-top: 0; }

        .li-number {
            text-align: right;
        }
        |]

contactMarkup :: ContactInfo -> (Localized String, Html)
contactMarkup = \case
    Bitbucket user      -> ("Bitbucket",  ahref "https://bitbucket.org/" user)
    EMail addr          -> (email,        ahref "mailto:" addr)
    Facebook user       -> ("Facebook",   ahref "https://" ("fb.me" </> user))
    GitHub user         -> ("GitHub",     ahref "https://github.com/" user)
    LinkedIn short      -> ("LinkedIn",   ahref "https://" ("linkedin.com/in/" </> short))
    Personal prefix url -> (personal,     ahref prefix url)
    Skype user          -> ("Skype",      ahref "callto:" user)
    Telegram user       -> ("Telegram",   ahref "https://telegram.me/" user)
    Telephone number    -> (tel,          toHtml number)
    Twitter user        -> ("Twitter",    ahref "https://twitter.com/" user)
  where
    ahref prefix url = a ! href (toValue (prefix <> url)) $ toHtml url
    email = Localized $ \case
        En -> "E-mail"
        Ru -> "Эл. почта"
    personal = Localized $ \case
        En -> "Personal Web Page"
        Ru -> "Сайт"
    tel = Localized $ \case
        En -> "Tel."
        Ru -> "Тел."
