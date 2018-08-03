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
  deriving (Show)

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

data BefungeProgram = Prog {
  origin :: BefungeCell
  , maxRows :: Int
  , maxCols :: Int
  }

