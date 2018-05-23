{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Data.CV.Render where

import           Control.Monad                 (forM_, unless)
import           Data.ByteString.Lazy          (ByteString)
import           Data.CV.Types                 (CV(..), ContactInfo(..),
                                                Education(..), Locale(En, Ru),
                                                Localized, Work(..), showRu)
import qualified Data.List                     as List
import           Data.Monoid                   ((<>))
import           System.FilePath               ((</>))
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Blaze.Html5              as T
import           Text.Blaze.Html5.Attributes   as A
import           Text.Shakespeare.Text         (st)

renderCv :: Locale -> CV -> ByteString
renderCv locale CV {..} =
    renderHtml . docTypeHtml $ do
        T.head $ do
            meta ! charset "UTF-8"
            T.title . toHtml $ localize fullname
            T.style styles
        body $ do
            T.div ! class_ "contact-info" $
                table $
                -- tr $
                --     td ! colspan (toValue (2 :: Int)) $
                --     img ! src (toValue photo)
                 do
                    tr $
                        td ! colspan (toValue (2 :: Int)) $
                        h3 $
                        localize $ \case En -> "Contact Info"; Ru -> "Контакты"
                    forM_ contactInfo $ \(contactMarkup -> (contactLabel, contactContent)) ->
                        tr $ do
                            td $ toHtml $ localize contactLabel
                            td contactContent
            h1 . toHtml $ localize fullname
            hr !
                A.style
                    "height: 0; border-top: solid 1px black; border-bottom: none;"
            h3 . localize $ \case
                En -> "Professional Skills"; Ru -> "Навыки и умения"
            dl . dd $ localize professionalSkills
            h4 . localize $ \case
                En -> "Technologies and Languages"; Ru -> "Технологии и языки"
            ul . forM_ technologies $ \(techGroup, tech) ->
                li $ do
                    em . toHtml $ localize techGroup
                    " "
                    toHtml $
                        List.intercalate ", " (List.map localize tech) <> "."
            h3 $ localize $ \case En -> "Work Experience"; Ru -> "Опыт работы"
            table ! class_ "work" $
                forM_ workExperience $ \Work {..} ->
                    tr $ do
                        td $ do
                            localize $ \case
                                En ->
                                    case workEnd of
                                        Nothing ->
                                            "started " >> timeSpan workStart
                                        Just end -> do
                                            timeSpan workStart
                                            preEscapedString "&nbsp;— "
                                            timeSpan end
                                Ru -> do
                                    timeSpan workStart
                                    preEscapedString "&nbsp;— "
                                    case workEnd of
                                        Nothing ->
                                            abbr ! A.title "настоящее время" $
                                            nobr "н. в."
                                        Just end -> timeSpan end
                            br
                            toHtml $ "(" <> localize totalTime <> ")"
                        td $ do
                            toHtml $ localize position
                            br
                            localize $ \case
                                En -> "at "
                                Ru -> ""
                            T.span ! class_ "place" $
                                toHtml $ localize organization
                            ", "
                            toHtml $ localize location
                            localize description
            h3 $ localize $ \case En -> "Education"; Ru -> "Образование"
            table ! class_ "edu" $
                forM_ education $ \Education {..} ->
                    tr $ do
                        td $ T.span ! class_ "time" $
                            if graduated > 0 then
                                toHtml graduated
                            else
                                toHtml $ "(" <> show (negate graduated) <> ")"
                        td $ do
                            T.span ! class_ "place" $ toHtml $ localize school
                            let division' = localize division
                            unless (null division') $ do
                                ","
                                br
                                toHtml $ localize division
                        td ! class_ "degree" $ toHtml $ localize degree
            h3 . localize $ \case
                En -> "Public Activity"; Ru -> "Общественная деятельность"
            table ! class_ "achiev" $
                forM_ publicActivity $ \((year, month), description) ->
                    tr $ do
                        td . p $ timeSpan (year, month)
                        td $ localize description
            h4 . localize $ \case En -> "Talks"; Ru -> "Выступления"
            table ! class_ "achiev" $
                forM_ talks $ \((year, month), description) ->
                    tr $ do
                        td . p $ timeSpan (year, month)
                        td $ localize description
            h3 . localize $ \case En -> "Residence"; Ru -> "Место жительства"
            dl . dd $ localize residence
  where
    localize f = f locale
    timeSpan (year, month) =
        T.span ! class_ "time" $ do
            toHtml $ localize $ \case En -> show month; Ru -> showRu month
            " "
            toHtml year
    nobr = T.span ! A.style "white-space: nowrap;"
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
contactMarkup =
    \case
        Bitbucket user -> ("Bitbucket", ahref "https://bitbucket.org/" user)
        EMail addr -> (email, ahref "mailto:" addr)
        Facebook user -> ("Facebook", ahref "https://" ("fb.me" </> user))
        GitHub user -> ("GitHub", ahref "https://github.com/" user)
        LinkedIn short ->
            ("LinkedIn", ahref "https://" ("linkedin.com/in/" </> short))
        Personal prefix url -> (personal, ahref prefix url)
        Skype user -> ("Skype", ahref "callto:" user)
        Telegram user -> ("Telegram", ahref "https://telegram.me/" user)
        Telephone number -> (tel, toHtml number)
        Twitter user -> ("Twitter", ahref "https://twitter.com/" user)
  where
    ahref prefix url = a ! href (toValue (prefix <> url)) $ toHtml url
    email En = "E-mail"
    email Ru = "Эл. почта"
    personal En = "Personal Web Page"
    personal Ru = "Сайт"
    tel En = "Tel."
    tel Ru = "Тел."
