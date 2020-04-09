module LLang where

import Combinators (Parser (..))
import           AST         (AST (..), Operator (..), Subst (..))
import           Data.List   (intercalate)
import Expr 
import Data.Map (Map (..))
import Control.Applicative ((<|>), many)
import qualified Data.Map    as Map
import           Text.Printf (printf)

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int] }
                   deriving (Show, Eq)

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Eq)

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

initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []

evalAST :: AST -> Subst -> Maybe Int
evalAST (Num x) _ = Just x
evalAST (Ident v) s = Map.lookup v s
evalAST (UnaryOp op x) s = do 
  v <- evalAST x s
  return $ compute (UnaryOp op (Num v))
evalAST (BinOp op x y) s = do 
  l <- evalAST x s
  r <- evalAST y s
  return $ compute (BinOp op (Num l) (Num r))

eval :: LAst -> Configuration -> Maybe Configuration
eval st@(If cond tr fls) config@(Conf subst i o) = do 
  c <- evalAST cond subst
  if (c /= 0) then (eval tr config) else (eval fls config)

eval st@(Assign var expr) config@(Conf subst i o) = do 
  e <- evalAST expr subst
  return $ Conf (Map.insert var e subst) i o

eval st@(Read var) config@(Conf subst (h:i) o) = return $ Conf (Map.insert var h subst) i o
eval st@(Read var) config@(Conf subst [] o) = Nothing

eval st@(Write expr) config@(Conf subst i o) = do 
  e <- evalAST expr subst
  return $ Conf subst i (e:o)

eval st@(Seq []) config@(Conf subst i o) = Just config
eval st@(Seq (s:stx)) config@(Conf subst i o) = do 
  nc <- eval s config
  eval (Seq stx) nc

eval st@(While cond seq) config@(Conf subst i o) = do 
  expr <- evalAST cond subst
  if (expr == 0) then return config else do {nc <- eval seq config; eval st nc}

instance Show LAst where
  show =
      go 0
    where
      go n t =
        let makeIdent = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id in

        case t of
          If cond thn els -> makeIdent $ printf "if %s\n%sthen\n%s\n%selse\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) thn) (makeIdent "") (go (ident n) els)
          While cond body -> makeIdent $ printf "while %s\n%sdo\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) body)
          Assign var expr -> makeIdent $ printf "%s := %s" var (flatShowExpr expr)
          Read var        -> makeIdent $ printf "read %s" var
          Write expr      -> makeIdent $ printf "write %s" (flatShowExpr expr)
          Seq stmts       -> intercalate "\n" $ map (go n) stmts
      ident = (+1)
      flatShowExpr (BinOp op l r) = printf "(%s %s %s)" (flatShowExpr l) (show op) (flatShowExpr r)
      flatShowExpr (UnaryOp op x) = printf "(%s %s)" (show op) (flatShowExpr x)
      flatShowExpr (Ident x) = x
      flatShowExpr (Num n) = show n
