module Language.Befunge.Lexer where
import Language.Befunge.Syntax
import Text.ParserCombinators.Parsec hiding (token, tokens)
import Control.Applicative ((<*), (*>), (<*>), (<$>))
import Data.Char (digitToInt)

type TokenPos = (BefungeOp, SourcePos)

parsePos :: Parser BefungeOp -> Parser TokenPos
parsePos p = (,) <$> p <*> getPosition

opCharList = [
  (Add, '+')
  , (Sub, '-')
  , (Mul, '*')
  , (Div, '/')
  , (Mod, '%')
  , (Not, '!')
  , (Gt, '`')
  , (MvR, '>')
  , (MvL, '<')
  , (MvU, '^')
  , (MvD, 'v')
  , (MvRand, '?')
  , (PopMvR, '_')
  , (PopMvD, '|')
  , (Str, '"')
  , (Dup, ':')
  , (Swap, '\\')
  , (Pop, '$')
  , (PopPrnt, '.')
  , (PopPrntChar, ',')
  , (Bridge, '#')
  , (Put, 'p')
  , (Get, 'g')
  , (ReadStdIO, '&')
  , (ReadStdIOChar, '~')
  , (End, '@')
  , (NOp, ' ')
            ]

numToken :: Parser TokenPos
numToken = parsePos $ digit >>= (\ charD -> return $ Num (digitToInt charD))

token :: Parser TokenPos
token = choice
  (numToken: otherParsers)
  where otherParsers = map tupToParser opCharList
        tupToParser :: (BefungeOp, Char) -> Parser TokenPos
        tupToParser (con, c) = parsePos $ char c >> return con
