{-# LANGUAGE RecordWildCards, QuasiQuotes #-}

module Data.CV.Render where

import Data.CV.Types
import Text.Blaze.Html.Renderer.Pretty
import Text.Hamlet

renderCv :: Language -> CV -> String
renderCv lang CV{..} = renderHtml [shamlet|
    $doctype 5
    <html>
        <head>
            <title>#{name lang}
            <style>
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
        <body>
            <.contact-info>
                <table>
                    <tr>
                        <td colspan=2>
                            <img src=#{photo}>
    |]
