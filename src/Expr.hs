module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), elem', fail',
                              satisfy, some', symbol)
import           Data.Char   (digitToInt, isDigit)
import           Control.Monad
import           Control.Applicative

data Associativity
  = LeftAssoc  -- 1 @ 2 @ 3 @ 4 = (((1 @ 2) @ 3) @ 4)
  | RightAssoc -- 1 @ 2 @ 3 @ 4 = (1 @ (2 @ (3 @ 4))
  | NoAssoc    -- Может быть только между двумя операндами: 1 @ 2 -- oк; 1 @ 2 @ 3 -- не ок

-- Универсальный парсер выражений
uberExpr :: Monoid e
         => [(Parser e i op, Associativity)] -- список парсеров бинарных операторов с ассоциативностями в порядке повышения приоритета
         -> Parser e i ast -- парсер для элементарного выражения
         -> (op -> ast -> ast -> ast) -- функция для создания абстрактного синтаксического дерева для бинарного оператора
         -> Parser e i ast
uberExpr operators pE build = foldr f pE operators where
	f (op, assoc) pE = case assoc of
		NoAssoc -> do 
			e <- pE
			((`build` e) <$> op <*> pE) <|> return e
		LeftAssoc -> do 
			e <- pE
			lst <- many ((,) <$> op <*> pE)
			return $ foldl (\e1 (op, e2) -> build op e1 e2) e lst
		RightAssoc -> do 
			e <- pE
			lst <- many ((,) <$> op <*> pE)
			return (snd $ foldr1 (\(op1, e1) (op2, e2) -> (op1, build op2 e1 e2)) ((undefined, e):lst))

toParser c = symbol c >>= toOperator
operators = [(toParser '+' <|> toParser '-', LeftAssoc), (toParser '*' <|> toParser '/', LeftAssoc), (toParser '^', RightAssoc)]

-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = uberExpr operators ((Num <$> parseNum) <|> (symbol '(' *> parseExpr <* symbol ')')) BinOp
	

-- Парсер для натуральных чисел с 0
parseNum :: Parser String String Int
parseNum = foldl (\acc d -> 10 * acc + digitToInt d) 0 <$> go
  where
    go :: Parser String String String
    go = some' (satisfy isDigit)

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = elem' >>= toOperator

-- Преобразование символов операторов в операторы
toOperator :: Char -> Parser String String Operator
toOperator '+' = return Plus
toOperator '*' = return Mult
toOperator '-' = return Minus
toOperator '/' = return Div
toOperator _   = fail' "Failed toOperator"

evaluate :: String -> Maybe Int
evaluate input = do
  case runParser parseExpr input of
    Success rest ast | null rest -> return $ compute ast
    _                            -> Nothing

compute :: AST -> Int
compute (Num x)           = x
compute (BinOp Plus x y)  = compute x + compute y
compute (BinOp Mult x y)  = compute x * compute y
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Div x y)   = compute x `div` compute y

