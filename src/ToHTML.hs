{-# LANGUAGE OverloadedStrings #-}

module ToHTML where

import           Data.Text (Text)
import qualified Data.Text as T
import           MdParser  (ListItem (..), MdElem (..))

toHTML :: [MdElem] -> Text
toHTML = foldr (\el acc -> elemToHTML el <> acc) ""

elemToHTML :: MdElem -> Text
elemToHTML (PlainText text) = text
elemToHTML (Paragraph elements) = "<p>" <> toHTML elements <> "</p>"
elemToHTML (Heading level el) =
    "<h" <> T.pack (show level) <> ">" <> elemToHTML el <> "</h" <> T.pack (show level) <> ">"
elemToHTML (Bold el) = "<strong>" <> elemToHTML el <> "</strong>"
elemToHTML (Italic el) = "<em>" <> elemToHTML el <> "</em>"
elemToHTML (OrderedList items) =
    "<ol>" <> foldr (\(ListItem elements) acc -> "<li>" <> toHTML elements <> "</li>" <> acc) "" items <> "</ol>"
elemToHTML (UnorderedList items) =
    "<ul>" <> foldr (\(ListItem elements) acc -> "<li>" <> toHTML elements <> "</li>" <> acc) "" items <> "</ul>"
