module Brainfuck.Interpreter (
    State (..),
    initState,
    fromTape,
    mapCell,
    cell,
    replace,
    eval,
) where

import Foreign.Marshal.Unsafe (unsafeLocalState)
import Data.Char (ord, chr)
import Brainfuck.Parsing

data State = State {
    tape :: [Int],
    pos :: Int
} deriving (Show)

initState :: State
initState = State zeroes 0
    where zeroes = [ 0 | _ <- [0..] ]

fromTape :: [Int] -> State
fromTape xs = State xs 0

mapCell :: State -> (Int -> Int) -> State
mapCell (State xs i) f =
    let (x, y:ys) = splitAt i xs
        in State (x ++ f y : ys) i

replace :: State -> Int -> State
replace s x = mapCell s (const x)

cell :: State -> Int
cell (State xs i) =
    let (_, y:_) = splitAt i xs
    in y

shiftl :: State -> State
shiftl (State xs i) = State xs (pred i)

shiftr :: State -> State
shiftr (State xs i) = State xs (succ i)

eval :: State -> [Expr] -> State
eval s [Terminal t] =
    case t of Plus -> mapCell s succ
              Minus -> mapCell s pred
              MoveLeft -> shiftl s
              MoveRight -> shiftr s
              Input -> replace s (unsafeLocalState $ ord <$> getChar)
              Output -> mapCell s (\x -> unsafeLocalState $ putChar (chr x) >> return x)
eval s l@[Loop es] = 
    let s' = eval s es in
        if cell s' == 0
            then s'
            else eval s' l
eval s (t:ts) = eval (eval s [t]) ts
