{-# LANGUAGE ScopedTypeVariables #-}
module Language.Befunge.Eval where
import Control.Monad.State
import Language.Befunge.Stack
import Language.Befunge.Syntax
import Language.Befunge.Runner
import System.Random
import Language.Befunge.Lexer (toChar, singleToken)
import Data.Char (ord, chr)

iNot :: Int -> Int
iNot 0 = 1
iNot _ = 0

gt :: Int -> Int -> Int
gt b a = if b > a
         then 1
         else 0

randDir :: Int -> Dir
randDir 1 = U
randDir 2 = D
randDir 3 = L
randDir 4 = R

mvRand :: BefungeRunner ()
mvRand = do
  rN <- liftIO $ getStdGen >>= \ gen -> return $ fst (randomR (1,4) gen)
  let rD = randDir rN
  setIP (rD,1)

condMov :: Dir -> BefungeRunner IP
condMov d = do
  v <- popR
  if v == 0
    then return (d, 1)
    else return ((reflect d), 1)

eval :: BefungeProgram -> BefungeRunner ()
eval prog = run (origin prog)
  where run :: BefungeCell -> BefungeRunner ()
        run c = do
          currOp <- currentOp
          if currOp == End
            then return ()
            else do
            evalStep currOp
            mov
            currentCell >>= run

evalStep :: BefungeOp -> BefungeRunner ()
evalStep (Num i) = pushR i
evalStep Add = binOpR (+)
evalStep Sub = binOpR (-)
evalStep Mul = binOpR (*)
evalStep Div = binOpR div
evalStep Mod = binOpR mod
evalStep Not = unOpR iNot
evalStep Gt  = binOpR gt
evalStep MvR = setIP (R,1)
evalStep MvL = setIP (L,1) 
evalStep MvU = setIP (U,1) 
evalStep MvD = setIP (D,1)
evalStep MvRand = mvRand
evalStep PopMvR = (condMov R) >>= setIP
evalStep PopMvD = (condMov D) >>= setIP
evalStep Str = mov >> evalStr
evalStep Dup = dup
evalStep Swap = swap
evalStep Pop = popR >> return ()
evalStep PopPrnt = popR >>= (\ v -> liftIO $ print v)
evalStep PopPrntChar = popR >>= (\ v -> liftIO $ print (chr v))
evalStep Bridge = currentIP >>= (\ (d, _) -> setIP (d, 2))
evalStep Put = do
  pos <- currentPos
  y <- popR
  x <- popR
  v <- popR
  moveTo x y
  let newOp = singleToken (chr v)
  setOp newOp
  moveToPos pos
evalStep Get = do
  pos <- currentPos
  y <- popR
  x <- popR
  moveTo x y
  currChar <- currentOp >>= \op -> return (toChar op)
  pushR (ord currChar)
  moveToPos pos
evalStep ReadStdIO = do
  liftIO $ putStrLn "Please enter a number: "
  (input :: Int) <- liftIO $ readLn
  pushR input
evalStep ReadStdIOChar = do
  liftIO $ putStrLn "Please enter a character: "
  (input :: Char) <- liftIO $ readLn
  pushR (ord input)
evalStep NOp = return ()
evalStep End = return ()
evalStep _ = undefined
  
mov :: BefungeRunner ()
mov = do
  (dir, mag) <- currentIP
  case mag of
    0 -> setIP (dir, 1)
    n -> do
      nextCell <- fmap (cellInDir dir) currentCell
      case nextCell of
        Edge -> wrap dir
        _ -> stepDir (const nextCell) >> setIP (dir, (n-1))

wrap :: Dir -> BefungeRunner ()
wrap d = do
  nextCell <- fmap (cellInDir d) currentCell
  case nextCell of
    Edge -> setIP (d, 1)
    _ -> stepDir (cellInDir (reflect d)) >> wrap d

evalStr :: BefungeRunner ()
evalStr = do
  o <- currentOp
  let char = toChar o
  if (char == '"')
    then return ()
    else pushR (ord char) >> mov >> evalStr
