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

data OpType = Binary Associativity
            | Unary

uberExpr :: Monoid e
         => [(Parser e i op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
         -> Parser e i ast            -- парсер элементарного выражения
         -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
         -> (op -> ast -> ast)        -- конструктор узла для унарной операции
         -> Parser e i ast
uberExpr operators pE buildBin buildUn = foldr f pE operators where
  f (op, t) pE = case t of
    Unary -> (buildUn <$> op <*> pE) <|> pE
    Binary NoAssoc -> do 
			e <- pE
			((`buildBin` e) <$> op <*> pE) <|> return e
    Binary LeftAssoc -> do 
			e <- pE
			lst <- many ((,) <$> op <*> pE)
			return $ foldl (\e1 (op, e2) -> buildBin op e1 e2) e lst
    Binary RightAssoc -> do 
			(lst, e) <- (,) <$> (many ((,) <$> pE <*> op)) <*> pE
			return $ foldr (\(b, op) a -> buildBin op b a) e lst

toParser c = parseStr c >>= toOperator
operators = [(toParser "||", Binary RightAssoc), (toParser "&&", Binary RightAssoc), (toParser "==" <|> toParser "/=" <|> toParser "<=" <|> toParser "<" <|> toParser ">=" <|> toParser ">", Binary NoAssoc), (toParser "!", Unary), (toParser "+" <|> toParser "-", Binary LeftAssoc), (toParser "*" <|> toParser "/", Binary LeftAssoc), (toParser "-", Unary), (toParser "^", Binary RightAssoc)]



-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = uberExpr operators (Num <$> parseNum <|> Ident <$> parseIdent <|> symbol '(' *> parseExpr <* symbol ')') BinOp UnaryOp

-- Парсер для целых чисел
parseNum :: Parser String String Int
parseNum = foldl (\acc d -> 10 * acc + digitToInt d) 0 <$> go
  where
    go :: Parser String String String
    go = some (satisfy isDigit)


parseNegNum :: Parser String String Int
parseNegNum = Parser $ \input ->
  case input of 
    ('-':xs) -> case runParser parseNegNum xs of
      Success i r -> Success i (r * (-1))
      e -> e
    otherwise -> runParser parseNum input

parseIdent :: Parser String String String
parseIdent = ((:) <$> (satisfy isLetter <|> symbol '_')) <*> many (satisfy isLetter <|> satisfy isDigit <|> symbol '_')

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = foldr (\op lst -> ((parseStr op) >>= toOperator) <|> lst) empty ["+", "*", "-", "/=", "/", "==", "^", ">=", ">", "<=", "<", "&&", "||", "!"]

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
toOperator "!" = return Not
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
compute (BinOp Equal x y)  = fromEnum $ compute x == compute y
compute (BinOp Nequal x y) = fromEnum $ compute x /= compute y
compute (BinOp Ge x y)     = fromEnum $ compute x >= compute y
compute (BinOp Le x y)     = fromEnum $ compute x <= compute y
compute (BinOp Gt x y)     = fromEnum $ compute x > compute y
compute (BinOp Lt x y)     = fromEnum $ compute x < compute y
compute (BinOp Or x y)     = fromEnum $ (compute x /= 0) || (compute y /= 0)
compute (BinOp And x y)    = fromEnum $ (compute x /= 0) && (compute y /= 0)
compute (UnaryOp Minus x)  = -(compute x)
compute (UnaryOp Not x)    = fromEnum $ compute x == 0