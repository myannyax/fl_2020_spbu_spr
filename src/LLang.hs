module LLang where

import AST (AST (..), Operator (..))
import Combinators (Parser (..))
import Expr (parseExpr, parseStr, parseIdent)
import Control.Applicative ((<|>), many)
import Data.Map (Map (..))

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Map Var Int, input :: [Int], output :: [Int] }

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Show, Eq)

stmt :: LAst
stmt =
  Seq
    [ Read "X"
    , If (BinOp Gt (Ident "X") (Num 13))
         (Write (Ident "X"))
         (While (BinOp Lt (Ident "X") (Num 42))
                (Seq [ Assign "X"
                        (BinOp Mult (Ident "X") (Num 7))
                     , Write (Ident "X")
                     ]
                )
         )
    ]

parseL :: Parser String String LAst
parseL = parseAssign <|> parseRead <|> parseWrite <|> parseSeq <|> parseIf <|> parseWhile

parseSeq :: Parser String String LAst
parseSeq = do
  parseStr "{"
  many (parseStr " " <|> parseStr "\n")
  statements <- many parseStatement
  many (parseStr " " <|> parseStr "\n")
  parseStr "}"
  return (Seq statements)

parseWrite :: Parser String String LAst
parseWrite = do
    parseStr "write("
    many (parseStr " ")
    expr <- parseExpr
    many (parseStr " ")
    parseStr ")"
    return (Write expr)

parseRead :: Parser String String LAst
parseRead = do
    parseStr "read("
    many (parseStr " ")
    x <- parseIdent
    many (parseStr " ")
    parseStr ")"
    return (Read x)

parseAssign :: Parser String String LAst
parseAssign = do
  var <- parseIdent
  many (parseStr " ")
  parseStr ":="
  many (parseStr " ")
  expr <- parseExpr
  return (Assign var expr)

parseWhile :: Parser String String LAst
parseWhile = do
  parseStr "while("
  expr <- parseExpr
  parseStr ")"
  seq <- parseSeq
  return (While expr seq)

parseIf :: Parser String String LAst
parseIf = do
  parseStr "if("
  expr <- parseExpr
  parseStr ")"
  seqTrue <- parseSeq
  parseStr "else"
  seqFalse <- parseSeq
  return (If expr seqTrue seqFalse)


parseStatement = do 
  many (parseStr " " <|> parseStr "\n")
  st <- parseL
  parseStr ";"
  many (parseStr " " <|> parseStr "\n")
  return st

eval :: String -> Configuration -> Configuration
eval = error "eval not defined"
