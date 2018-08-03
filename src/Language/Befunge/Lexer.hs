module Language.Befunge.Lexer
  (
    tokenize
  , TokenPos
  , toChar
  , singleToken
  ) where
import Language.Befunge.Syntax
import Text.ParserCombinators.Parsec hiding (token, tokens)
import Text.Parsec.Error
import Text.Parsec.Char
import Data.Char (digitToInt)
import Data.Either (partitionEithers, rights)

type TokenPos = (BefungeOp, SourcePos)

parsePos :: Parser BefungeOp -> Parser TokenPos
parsePos p = (,) <$> p <*> (getPosition >>= (\ pos -> return $ incSourceColumn pos (-1)))

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

tupToParser :: (BefungeOp, Char) -> Parser BefungeOp
tupToParser (con, c) = char c >> return con

opParser :: Parser BefungeOp
opParser = choice $
  (numToken: otherParsers) ++ [otherToken]
  where numToken = digit >>= (\ charD -> return $ Num (digitToInt charD))
        otherToken = satisfy (\c -> c `notElem` "\n\r") >>= (\ c -> return $ Other c)
        otherParsers = map tupToParser opCharList

token :: Parser TokenPos
token = parsePos opParser

tokens :: Parser [TokenPos]
tokens = concat <$> many token `sepBy` endOfLine

tokenize :: SourceName -> String -> Either ParseError [TokenPos]
tokenize = runParser tokens ()

singleToken :: Char -> BefungeOp
singleToken c = (head.rights) [runParser opParser () "" [c]]
