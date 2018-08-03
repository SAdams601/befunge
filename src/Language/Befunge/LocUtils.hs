module Language.Befunge.LocUtils where
import Text.Parsec.Pos

upPos :: SourcePos -> SourcePos
upPos p = incSourceLine p (-1)

downPos :: SourcePos -> SourcePos
downPos p = incSourceLine p 1

rightPos :: SourcePos -> SourcePos
rightPos p = incSourceColumn p 1

leftPos :: SourcePos -> SourcePos
leftPos p = incSourceColumn p (-1)

posOutOfBounds :: Int -> Int -> SourcePos -> Bool
posOutOfBounds maxLines maxCols pos =
     (sourceLine pos) <= maxLines
  && (sourceColumn pos) <= maxCols 
