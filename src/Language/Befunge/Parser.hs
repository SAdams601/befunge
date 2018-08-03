module Language.Befunge.Parser where
import Language.Befunge.Syntax
import Language.Befunge.Lexer
import Text.ParserCombinators.Parsec (ParseError, SourcePos, sourceLine, sourceColumn)
import Control.Monad.State
import Data.Map hiding (foldl)

parse :: FilePath -> String -> (Either ParseError BefungeProgram)
parse fp source =
  (tokenize fp source) >>= (\ ts ->
      Right $ (\ (mr, mc) -> Prog (parseTokenStream ts) mr mc) (findBounds ts))

findBounds :: [TokenPos] -> (Int, Int)
findBounds = foldl maxPos (-1,-1)
  where maxPos (cMr, cMc) tPos = let pos = snd tPos in
          (max cMr (sourceLine pos), max cMc (sourceColumn pos))

parseTokenStream :: [TokenPos] -> BefungeCell
parseTokenStream = undefined

data ParseStatus = Sts {
  remainingTokens :: [TokenPos]
  , parsedCells :: Map SourcePos BefungeCell
  , numRows :: Int
  , numCols :: Int
                       }

type BefungeParseState = State ParseStatus

parseCell :: TokenPos -> BefungeParseState BefungeCell
parseCell trgt = do
  let trgtOp = fst trgt
      currRow = sourceLine $ snd trgt
      currCol = sourceColumn $ snd trgt
  return undefined
