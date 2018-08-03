{-# LANGUAGE RecursiveDo #-}
module Language.Befunge.Parser where
import Language.Befunge.Syntax
import Language.Befunge.Lexer
import Language.Befunge.LocUtils
import Text.ParserCombinators.Parsec (ParseError, SourcePos, sourceLine, sourceColumn)
import Control.Monad.Trans.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple (swap)

parse :: FilePath -> String -> Either ParseError BefungeProgram
parse fp source =
  (tokenize fp source) >>= (\ ts ->
      Right $ (\ (ml, mc) -> (Prog (parseTokenStream (ml, mc) ts) ml mc)) (findBounds ts))

findBounds :: [TokenPos] -> (Int, Int)
findBounds = foldl maxPos (-1,-1)
  where maxPos (cMr, cMc) tPos = let pos = snd tPos in
          (max cMr (sourceLine pos), max cMc (sourceColumn pos))

parseTokenStream :: (Int, Int) -> [TokenPos] -> BefungeCell
parseTokenStream (mr, mc) ts =
  let tsMap = tokenLocs ts
      initState = Sts tsMap Map.empty mr mc
  in evalState (parseCell ((snd.head) ts)) initState
      

data ParseStatus = Sts {
    tokens :: Map SourcePos BefungeOp
  , parsedCells :: Map SourcePos BefungeCell
  , numRows :: Int
  , numCols :: Int
                       }

tokenLocs :: [TokenPos] -> Map SourcePos BefungeOp
tokenLocs lst = Map.fromList (map swap lst)

type BefungeParseState = State ParseStatus

parseCell :: SourcePos -> BefungeParseState BefungeCell
parseCell sPos = do
  st <- get
  let mTrgtTok = Map.lookup sPos (tokens st)
  case mTrgtTok of
    Nothing -> return Edge
    Just trgtTok -> mdo
      let nCell = Cell sPos trgtTok upC downC leftC rightC
      modify (\ s -> Sts (tokens s) (Map.insert sPos nCell (parsedCells s)) (numRows s) (numCols s))
      upC <- getUp sPos
      downC <- getDown sPos
      leftC <- getLeft sPos
      rightC <- getRight sPos      
      return nCell

getUp :: SourcePos -> BefungeParseState BefungeCell
getUp = getCellDir upPos

getDown :: SourcePos -> BefungeParseState BefungeCell
getDown = getCellDir downPos

getLeft :: SourcePos -> BefungeParseState BefungeCell
getLeft = getCellDir leftPos

getRight :: SourcePos -> BefungeParseState BefungeCell
getRight = getCellDir rightPos

getCellDir :: (SourcePos -> SourcePos) -> SourcePos -> BefungeParseState BefungeCell
getCellDir transF pos = do
  st <- get
  let newPos = transF pos
      mCell = Map.lookup newPos (parsedCells st)
  case mCell of
    Just cell -> return cell
    Nothing ->  parseCell newPos  
  
prettyPrint :: BefungeProgram -> String
prettyPrint prog = f (origin prog)
  where f :: BefungeCell -> String
        f Edge = ""
        f cell = let line = f (right cell)
                     rest = f (down cell)
                     curr = case op cell of
                       Num i -> head $ show i
                       Other c -> c
                       o -> toChar o
                 in
                   if isLeftMostCell cell
                   then curr:(line ++ "\n" ++ rest)
                   else curr:line
