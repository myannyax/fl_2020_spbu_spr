module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), elem', fail',
                              satisfy, some', symbol)
import           Control.Monad
import           Control.Applicative
import           Data.Char   (digitToInt, isDigit, isLetter)

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
			(lst, e) <- (,) <$> (many ((,) <$> pE <*> op)) <*> pE
			return $ foldr (\(b, op) a -> build op b a) e lst

toParser c = parseStr c >>= toOperator
operators = [(toParser "||", RightAssoc), (toParser "&&", RightAssoc), (toParser "==" <|> toParser "/=" <|> toParser "<=" <|> toParser "<" <|> toParser ">=" <|> toParser ">", NoAssoc), (toParser "+" <|> toParser "-", LeftAssoc), (toParser "*" <|> toParser "/", LeftAssoc), (toParser "^", RightAssoc)]

-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = uberExpr operators ((Num <$> parseNum) <|> (Ident <$> parseIdent) <|> (symbol '(' *> parseExpr <* symbol ')')) BinOp

-- Парсер для целых чисел
parseNum' :: Parser String String Int
parseNum' = foldl (\acc d -> 10 * acc + digitToInt d) 0 <$> go
  where
    go :: Parser String String String
    go = some' (satisfy isDigit)


parseNum :: Parser String String Int
parseNum = Parser $ \input ->
  case input of 
    ('-':xs) -> case runParser parseNum xs of
      Success i r -> Success i (r * (-1))
      e -> e
    otherwise -> runParser parseNum' input

parseIdent :: Parser String String String
parseIdent = ((:) <$> (satisfy isLetter <|> symbol '_')) <*> many (satisfy isLetter <|> satisfy isDigit <|> symbol '_')

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = foldr (\op lst -> ((parseStr op) >>= toOperator) <|> lst) empty ["+", "*", "-", "/=", "/", "==", "^", ">=", ">", "<=", "<", "&&", "||"]

parseStr (x:xs) = do
  y <- symbol x
  ys <- parseStr xs
  return $ y:ys
parseStr [] = Parser $ \input -> Success input ""

-- Преобразование символов операторов в операторы
toOperator :: String -> Parser String String Operator
toOperator "+" = return Plus
toOperator "*" = return Mult
toOperator "-" = return Minus
toOperator "/" = return Div
toOperator "==" = return Equal
toOperator "^" = return Pow
toOperator "/=" = return Nequal
toOperator ">" = return Gt
toOperator ">=" = return Ge
toOperator "<" = return Lt
toOperator "<=" = return Le
toOperator "&&" = return And
toOperator "||" = return Or
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
compute (BinOp Pow x y)   = compute x ^ compute y