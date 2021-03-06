{-# LANGUAGE OverloadedStrings #-}

module Data.Hjq.Parser
    ( JqFilter (..)
    , parseJqFilter
    ) where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Text            (Text)
import qualified Data.Text            as Text

data JqFilter
    = JqField Text JqFilter
    | JqIndex Int JqFilter
    | JqNil
    deriving (Eq)

instance Read JqFilter
  where
    readsPrec _ s = case parseJqFilter (Text.pack s) of
        Right jf -> [(jf, "")]
        Left _   -> []

instance Show JqFilter
  where
    show JqNil = "."
    show (JqField s jf) = case jf of
        JqIndex i jf' ->
            "." ++ Text.unpack s ++ "[" ++ show i ++ "]" ++ show' jf'
        _ ->
            "." ++ Text.unpack s ++ show' jf
    show (JqIndex i jf) = "." ++ "[" ++ show i ++ "]" ++ show' jf

show' JqNil = ""
show' jf    = show jf

parseJqFilter :: Text -> Either Text JqFilter
parseJqFilter s = case parse (jqFilterParser <* endOfInput) s `feed` "" of
    Done _ jf -> Right jf
    jf        -> Left . Text.pack $ show jf

jqFilterParser :: Parser JqFilter
jqFilterParser = jqFilter <|> jqIdentity
  where
    jqFilter = jqIndexedField <|> jqField <|> jqIndex <|> jqNil

    jqIndexedField = toIndexedField <$ char '.'
        <*> fieldParser
        <*> indexParser
        <*> jqFilter
      where toIndexedField x y z = JqField x (JqIndex y z)

    jqField = JqField <$ char '.'
        <*> fieldParser
        <*> jqFilter

    jqIndex = JqIndex <$ char '.'
        <*> indexParser
        <*> jqFilter

    jqIdentity = char '.' *> jqNil

    jqNil = endOfInput *> pure JqNil

    fieldParser = Text.pack <$> many1 letter

    indexParser = char '[' *> (read <$> many1 digit) <* char ']'
