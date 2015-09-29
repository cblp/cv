{-# LANGUAGE  LambdaCase
            , OverloadedStrings
            , QuasiQuotes
            , RecordWildCards
            , ViewPatterns
  #-}

module Data.CV.Render where

import Control.Monad
import Data.CV.Types
import Data.Monoid
import System.FilePath
import Text.Blaze.Html.Renderer.Pretty
import Text.Blaze.Html5 as Tag
import Text.Blaze.Html5.Attributes as Attr
import Text.Shakespeare.Text

renderCv :: Locale -> CV -> String
renderCv locale CV{..} = renderHtml . docTypeHtml $ do
    Tag.head $ do
        Tag.title . toHtml $ fullname locale
        Tag.style styles
    body $
        Tag.div ! class_ "contact-info" $
            table $ do
                tr $ td ! colspan (toValue (2 :: Int)) $
                    img ! src (toValue photo)
                tr $ td ! colspan (toValue (2 :: Int)) $
                    h3 "Contact Info"
                forM_ contactInfo $ \(contactMarkup -> (contactLabel, contactContent)) ->
                    tr $ do
                        td $ toHtml contactLabel
                        td contactContent
  where
    styles :: Html
    styles = toHtml [st|
        body {
            font-family: Georgia, serif;
            padding: 3em;
        }

        h1, h2 {
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

        .edu .time {
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

contactMarkup :: ContactInfo -> (String, Html)
contactMarkup = \case
    Bitbucket user          -> ("Bitbucket",          ahref "https://bitbucket.org/" user)
    EMail addr              -> ("e-mail",             ahref "mailto:" addr)
    Facebook user           -> ("Facebook",           ahref "https://" ("fb.me" </> user))
    GitHub user             -> ("GitHub",             ahref "https://github.com/" user)
    LinkedIn short          -> ("LinkedIn",           ahref "https://" ("linkedin.com/in/" </> short))
    PersonalPage prefix url -> ("Personal web page",  ahref prefix url)
    Skype user              -> ("Skype",              ahref "callto:" user)
    Telegram user           -> ("Telegram",           ahref "https://telegram.me/" user)
    Telephone number        -> ("tel.",               toHtml number)
    Twitter user            -> ("Twitter",            ahref "https://twitter.com/" user)
  where
    ahref :: String -> String -> Html
    ahref prefix url = a ! href (toValue (prefix <> url)) $ toHtml url
