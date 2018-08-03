{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Befunge.Runner where
import Language.Befunge.Syntax
import Language.Befunge.Stack
import Control.Monad.Trans.State
import Control.Monad.State.Class (MonadState)
import Control.Monad.IO.Class 
import Text.Parsec.Pos (sourceLine, sourceColumn, SourcePos)

data Dir = U
         | D
         | L
         | R

reflect :: Dir -> Dir
reflect U = D
reflect D = U
reflect L = R
reflect R = L

type IP = (Dir, Int)

data BefungeState = BSt {
    location :: BefungeCell
  , stack :: Stack Int
  , ip :: IP
                        }

initState :: BefungeCell -> BefungeState
initState prog = BSt prog Empty (R,1)

type BefungeRunner a = StateT BefungeState IO a

runBefunge :: BefungeCell -> BefungeRunner a -> IO (a, BefungeState)
runBefunge prog runner = runStateT runner (initState prog)

applyToStack :: (Stack Int -> Stack Int) -> BefungeRunner ()
applyToStack f = modify (\st -> st { stack = f (stack st)})

pushR :: Int -> BefungeRunner ()
pushR i = applyToStack $ push i

popR :: BefungeRunner Int
popR = state (\ st -> let (v,s) = pop (stack st) in
               (v, st {stack = s}))

unOpR :: (Int -> Int) -> BefungeRunner ()
unOpR f = applyToStack $ unOp f

binOpR :: (Int -> Int -> Int) -> BefungeRunner ()
binOpR f = applyToStack $ binOp f

currentCell :: BefungeRunner BefungeCell
currentCell = get >>= \ st -> return (location st)

currentOp :: BefungeRunner BefungeOp
currentOp = currentCell >>= \ l -> return (op l)

currentPos :: BefungeRunner SourcePos
currentPos = currentCell >>= \ cell -> return (loc cell)

currentIP :: BefungeRunner IP
currentIP = get >>= \ st -> return (ip st)
  
setIP :: IP -> BefungeRunner ()
setIP nIp = modify (\ st -> st { ip = nIp})

setOp :: BefungeOp -> BefungeRunner ()
setOp o = do
  currCell <- currentCell
  let newCell = currCell { op = o}
  modify (\ st -> st { location = newCell })

dup :: BefungeRunner ()
dup = popR >>= (\ i -> pushR i >> pushR i)

swap :: BefungeRunner ()
swap = popR >>= (\ v1 -> popR >>= (\ v2 -> pushR v1 >> pushR v2))

stepDir :: (BefungeCell -> BefungeCell) -> BefungeRunner ()
stepDir f = modify (\ st -> st { location = f (location st)})

moveToPos :: SourcePos -> BefungeRunner ()
moveToPos sp = moveTo (sourceLine sp) (sourceColumn sp)

moveTo :: Int -> Int -> BefungeRunner ()
moveTo ln col = do
  cell <- currentCell
  let currLine = sourceLine (loc cell)
      currCol = sourceColumn (loc cell)
      lnsToMv = ln - currLine
      colsToMv = col - currCol
  moveLines lnsToMv >> moveRows colsToMv

moveLines :: Int -> BefungeRunner ()
moveLines 0 = return ()
moveLines lns = if lns < 0
                then stepDir up >> moveLines (lns+1)
                else stepDir down >> moveLines (lns-1)

moveRows :: Int -> BefungeRunner ()
moveRows 0 = return ()
moveRows rws = if rws < 0
                then stepDir left >> moveLines (rws+1)
                else stepDir right >> moveLines (rws-1)
