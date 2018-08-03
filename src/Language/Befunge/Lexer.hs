module Language.Befunge.Lexer
  (
    tokenize
  , TokenPos
  , toChar
  )where
import Language.Befunge.Syntax
import Text.ParserCombinators.Parsec hiding (token, tokens)
import Text.Parsec.Error
import Text.Parsec.Char
import Data.Char (digitToInt)
import Data.Either (partitionEithers)

type TokenPos = (BefungeOp, SourcePos)

parsePos :: Parser BefungeOp -> Parser TokenPos
parsePos p = (,) <$> p <*> getPosition

toChar :: BefungeOp -> Char
toChar (Other c) = c
toChar (Num i) = head . show $ i
toChar o = snd . head $ filter (\ (op2, _) -> o == op2) opCharList

opCharList :: [(BefungeOp, Char)]
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

otherToken :: Parser TokenPos
otherToken = parsePos $ satisfy (\c -> c `notElem` "\n\r") >>= (\ c -> return $ Other c)

token :: Parser TokenPos
token = choice $
  (numToken: otherParsers) ++ [otherToken]
  where otherParsers = map tupToParser opCharList
        tupToParser :: (BefungeOp, Char) -> Parser TokenPos
        tupToParser (con, c) = parsePos $ char c >> return con

tokens :: Parser [TokenPos]
tokens = concat <$> many token `sepBy` endOfLine

tokenize :: SourceName -> String -> Either ParseError [TokenPos]
tokenize = runParser tokens ()
