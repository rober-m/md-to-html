{-# LANGUAGE OverloadedStrings #-}

module MdParser where

import           Control.Monad
import           Data.Text            (Text)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

type MDParser = Parsec Void Text

newtype ListItem = ListItem [MdElem]
    deriving (Show, Eq)

data MdElem
    = PlainText Text
    | Bold MdElem
    | Italic MdElem
    | Heading Int MdElem
    | Paragraph [MdElem]
    | OrderedList [ListItem]
    | UnorderedList [ListItem]
    deriving (Show, Eq)

-- 1
parseText :: MDParser MdElem
parseText = PlainText <$> takeWhile1P (Just "plain text") (`notElem` ['*', '_', '\n'])

-- 2 (with `where` clause using `between` instead of `parseBetween`. Replace when `parseBetween` is defined.)
parseBold :: MDParser MdElem
parseBold = label "bold text" $ do
    Bold <$> (parseBetween "**" <|> parseBetween "__")

-- 3 (with `where` clause using `between` instead of `parseBetween`. Replace when `parseBetween` is defined.)
parseItalic :: MDParser MdElem
parseItalic = label "italic text" $ do
    Italic <$> (parseBetween "*" <|> parseBetween "_")

-- 4 (with `parseText` instead of `parseLineElement`. Replace when `parseLineElement` is defined)
parseBetween :: Text -> MDParser MdElem
parseBetween str = between (chunk str) (chunk str) parseLineElement

-- 5
parseLineElement :: MDParser MdElem
parseLineElement =
    choice
        [ parseBold
        , parseItalic
        , parseText
        ]

-- 6
parseHeading :: MDParser MdElem
parseHeading = label "heading" $ do
    level <- length <$> some (char '#')
    when (level > 6) $
        fail "heading level must be between 1 and 6"
    space1
    Heading level <$> parseLineElement

-- 7
parseParagraph :: MDParser MdElem
parseParagraph = label "paragraph" $ do
    skipSpaces
    elements <- some parseLineElement
    skipSpaces
    pure $ Paragraph elements

-- 8 (define inside 6 and extract it)
skipSpaces :: MDParser ()
skipSpaces = void $ many (space1 <|> void newline)

-- 9
parseOrderedList :: MDParser MdElem
parseOrderedList = label "ordered list" $ do
    OrderedList <$> some parseListItem
  where
    parseListItem :: MDParser ListItem
    parseListItem = do
        void $ digitChar >> char '.' >> space1
        elements <- some parseLineElement
        skipSpaces
        pure $ ListItem elements

-- 10
parseUnorderedList :: MDParser MdElem
parseUnorderedList = label "unordered list" $ do
    UnorderedList <$> some parseListItem
  where
    parseListItem :: MDParser ListItem
    parseListItem = do
        void $ oneOf ['-', '*', '+'] >> space1
        elements <- some parseLineElement
        skipSpaces
        pure $ ListItem elements


-- 11
parseGroupElement :: MDParser MdElem
parseGroupElement =
    choice
        [ parseHeading
        , parseOrderedList
        , parseUnorderedList
        , parseParagraph
        ]

-- 12
mainParser :: MDParser [MdElem]
mainParser = do
    skipSpaces
    elements <- some parseGroupElement
    eof
    pure elements
