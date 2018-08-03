module Language.Befunge.Syntax where
import Text.ParserCombinators.Parsec (SourcePos)

data BefungeOp =
    Num Int
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Not
  | Gt
  | MvR
  | MvL
  | MvU
  | MvD
  | MvRand
  | PopMvR
  | PopMvD
  | Str
  | Dup
  | Swap
  | Pop
  | PopPrnt
  | PopPrntChar
  | Bridge
  | Put
  | Get
  | ReadStdIO
  | ReadStdIOChar
  | End
  | NOp
  | Other Char
  deriving (Show, Eq)

data BefungeCell =
    Edge
  | Cell {
      loc :: SourcePos
      , op :: BefungeOp
      , up :: BefungeCell
      , down :: BefungeCell
      , left :: BefungeCell
      , right :: BefungeCell
      }
    deriving Eq

isLeftMostCell :: BefungeCell -> Bool
isLeftMostCell cell = Edge == left cell
             
instance Show BefungeCell where
  show Edge = "Edge"
  show cell = let curr = show (op cell)
                  row = show (right cell)
                  rest = show (down cell) in
              if isLeftMostCell cell
              then "Edge " ++ curr ++ " " ++ row ++ "\n" ++ rest
              else curr ++ " " ++ row


data BefungeProgram = Prog {
  origin :: BefungeCell
  , maxRows :: Int
  , maxCols :: Int
  }

instance Show BefungeProgram where
  show prog = show (origin prog)

cellCount :: BefungeProgram -> Int
cellCount prog = f (origin prog)
  where f Edge = 0
        f cell = 1 + f (right cell) + f (down cell)
