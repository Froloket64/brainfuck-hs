module Brainfuck.Parsing (
    Expr (..),
    Operator (..),
    token,
    expr,
) where

import Text.Parsec
import GHC.Base (IO(IO))

-- +-><[],.
-- "++ [.-] > ,. < test"

data Expr = Terminal Operator
          | Loop [Expr]
          deriving (Show)

data Operator = Plus
              | Minus
              | MoveLeft
              | MoveRight
              | Input
              | Output
              deriving (Show)

terminal :: Parsec String st Expr
terminal = Terminal . match <$> oneOf "+-><,."
    where
        match '+' = Plus
        match '-' = Minus
        match '<' = MoveLeft
        match '>' = MoveRight
        match ',' = Input
        match '.' = Output

letters = oneOf ['a'..'z'] <|> oneOf ['A'..'Z']
digits = oneOf ['0'..'9']
whitespace = oneOf " \n\r\t"

loop :: Parsec String st Expr
loop = Loop <$> between (char '[') (char ']') (many (many whitespace *> (terminal <|> loop)))

expr :: Parsec String st [Expr]
expr = many (many (letters <|> digits <|> whitespace) *> (terminal <|> loop))
