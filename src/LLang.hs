module LLang where

<<<<<<< HEAD
import AST (AST (..), Operator (..), Subst (..))
import Combinators (Parser (..))
<<<<<<< HEAD
import Expr (parseExpr, parseStr, parseIdent)
import Control.Applicative ((<|>), many)
import Data.Map (Map (..))
=======
import qualified Data.Map as Map
>>>>>>> HW07 tests
=======
import           AST         (AST (..), Operator (..), Subst (..))
import           Combinators (Parser (..))
import           Data.List   (intercalate)
import qualified Data.Map    as Map
import           Text.Printf (printf)
>>>>>>> Nicer Show for LAst

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

eval :: LAst -> Configuration -> Maybe Configuration
eval = error "eval not defined"

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
