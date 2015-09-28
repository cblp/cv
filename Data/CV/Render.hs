module Data.CV.Render where

import Data.CV.Types
import Text.Blaze.Html.Renderer.Pretty
import Text.Blaze.Html5

renderCv :: Language -> CV -> String
renderCv _ _ = renderHtml . docTypeHtml $ do
    return ()
