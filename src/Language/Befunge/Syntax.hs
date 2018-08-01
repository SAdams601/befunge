module Language.Befunge.Syntax where

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
  
type Coords = (Int, Int)

data BefungeCell = Cell {
    loc :: Coords
  , op :: BefungeOp
  , up :: BefungeCell
  , down :: BefungeCell
  , left :: BefungeCell
  , right :: BefungeCell
  }

