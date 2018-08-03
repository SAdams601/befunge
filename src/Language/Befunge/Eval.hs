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

eval :: BefungeOp -> BefungeRunner ()
eval (Num i) = pushR i
eval Add = binOpR (+)
eval Sub = binOpR (-)
eval Mul = binOpR (*)
eval Div = binOpR div
eval Mod = binOpR mod
eval Not = unOpR iNot
eval Gt  = binOpR gt
eval MvR = setIP (R,1)
eval MvL = setIP (L,1) 
eval MvU = setIP (U,1) 
eval MvD = setIP (D,1)
eval MvRand = mvRand
eval PopMvR = (condMov R) >>= setIP
eval PopMvD = (condMov D) >>= setIP
eval Str = mov >> evalStr
eval Dup = dup
eval Swap = swap
eval Pop = popR >> return ()
eval PopPrnt = popR >>= (\ v -> liftIO $ print v)
eval PopPrntChar = popR >>= (\ v -> liftIO $ print (chr v))
eval Bridge = currentIP >>= (\ (d, _) -> setIP (d, 2))
eval Put = do
  pos <- currentPos
  y <- popR
  x <- popR
  v <- popR
  moveTo x y
  let newOp = singleToken (chr v)
  setOp newOp
  moveToPos pos
eval Get = do
  pos <- currentPos
  y <- popR
  x <- popR
  moveTo x y
  currChar <- currentOp >>= \op -> return (toChar op)
  pushR (ord currChar)
  moveToPos pos
eval ReadStdIO = do
  liftIO $ putStrLn "Please enter a number: "
  (input :: Int) <- liftIO $ readLn
  pushR input
eval ReadStdIOChar = do
  liftIO $ putStrLn "Please enter a character: "
  (input :: Char) <- liftIO $ readLn
  pushR (ord input)
eval NOp = return ()
eval End = return ()
eval _ = undefined
  
mov :: BefungeRunner ()
mov = undefined

evalStr :: BefungeRunner ()
evalStr = do
  o <- currentOp
  let char = toChar o
  if (char == '"')
    then return ()
    else pushR (ord char) >> mov >> evalStr
