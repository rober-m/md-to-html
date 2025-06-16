{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Monad (void, when)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, newline, space1)
import Prelude hiding (lines)

type MDParser = Parsec Void Text

newtype ListItem = ListItem [MdElem]
    deriving (Show, Eq)

data MdElem
    = PlainText Text
    | Paragraph [MdElem]
    | Bold MdElem
    | Italic MdElem
    | Heading Int MdElem
    | OrderedList [ListItem]
    | UnorderedList [ListItem]
    deriving (Show, Eq)

mainParser :: MDParser [MdElem]
mainParser = do
    skipSpaces
    elements <- some parseGroupElement
    eof
    pure elements

skipSpaces :: MDParser ()
skipSpaces = void $ many (space1 <|> void newline)

parseGroupElement :: MDParser MdElem
parseGroupElement =
    choice
        [ parseHeading
        , parseOrderedList
        , parseUnorderedList
        , parseParagraph
        ]

parseLineElement :: MDParser MdElem
parseLineElement =
    choice
        [ parseBold
        , parseItalic
        , parseText
        ]

parseHeading :: MDParser MdElem
parseHeading = label "heading" $ do
    level <- length <$> some (char '#')
    when (level > 6) $
        fail "heading level must be between 1 and 6"
    space1
    Heading level <$> parseLineElement

parseParagraph :: MDParser MdElem
parseParagraph = label "paragraph" $ do
    skipSpaces
    elements <- some parseLineElement
    skipSpaces
    pure $ Paragraph elements

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

parseBetween :: Text -> MDParser MdElem
parseBetween str = between (chunk str) (chunk str) parseLineElement

parseBold :: MDParser MdElem
parseBold = label "bold text" $ do
    Bold <$> (parseBetween "**" <|> parseBetween "__")

parseItalic :: MDParser MdElem
parseItalic = label "italic text" $ do
    Italic <$> (parseBetween "*" <|> parseBetween "_")

parseText :: MDParser MdElem
parseText = PlainText <$> takeWhile1P (Just "plain text") (`notElem` ['*', '_', '\n'])
