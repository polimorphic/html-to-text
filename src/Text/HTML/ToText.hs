{-# LANGUAGE OverloadedStrings, TypeApplications #-}

module Text.HTML.ToText (htmlToText) where

import Data.Bool (bool)
import Data.Foldable (fold)
import Data.Function (on)
import Data.List (groupBy)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Text.HTML.TagSoup (Tag(TagClose, TagOpen, TagText), parseTags)

htmlToText :: Text -> Text
htmlToText html
    = T.intercalate "\n"
    . filter (not . T.null)
    . fmap (T.dropAround (== ' '))
    . mapMaybe (fmap fold . sequenceA)
    . groupBy ((==) `on` null)
    $ tagToText =<< parseTags html

tagToText :: Tag Text -> [Maybe Text]
tagToText (TagOpen "br" _) = [Just "\n"]
tagToText (TagOpen nm _) | S.notMember nm phrasing = [Nothing]
tagToText (TagClose nm) | S.notMember nm phrasing = [Nothing]
tagToText (TagText txt) = [Just $ collapse txt]
tagToText _ = []

collapse :: Text -> Text
collapse = T.pack . go False . T.unpack
  where
    go clps (c : cs)
        | elem @[] c "\t\n " = bool (' ' :) id clps $ go True cs
        | otherwise = c : go False cs
    go _ [] = ""

phrasing :: Set Text
phrasing = S.fromList
    [ "a", "abbr", "area", "audio", "b", "bdo", "br", "button", "canvas", "cite", "code", "command"
    , "data", "datalist", "del", "dfn", "em", "embed", "i", "iframe", "img", "input", "ins", "kbd"
    , "keygen", "label", "link", "map", "mark", "math", "meta", "meter", "noscript", "object"
    , "output", "picture", "progress", "q", "ruby", "samp", "script", "select", "small", "span"
    , "strong", "sub", "sup", "svg", "textarea", "time", "u", "var", "video", "wbr"
    ]
