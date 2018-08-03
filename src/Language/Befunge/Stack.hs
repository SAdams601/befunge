module Language.Befunge.Stack where
import Control.Monad.Trans.State

data Stack a =
    Empty
  | Item a (Stack a)

push :: a -> Stack a -> Stack a
push = Item 

pop :: Stack a -> (a, Stack a)
pop Empty = undefined
pop (Item v s) = (v, s)

isEmpty :: Stack a -> Bool
isEmpty Empty = True
isEmpty _ = False

unOp :: (a -> a) -> Stack a -> Stack a
unOp op s = let (a,s2) = pop s
                nv = op a
            in
              push nv s2

binOp :: (a -> a -> a) -> Stack a -> Stack a
binOp op s = let (a, s2) = pop s
                 (b, s3) = pop s2
             in
               push (b `op` a) s3

