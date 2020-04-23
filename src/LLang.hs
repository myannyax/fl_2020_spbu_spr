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

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int], defs :: Defs } deriving (Show, Eq)

type Defs = Map.Map String Function

data Program = Program { functions :: [Function], main :: LAst }

data Function = Function { name :: String, args :: [Var], funBody :: LAst, returnExpr :: Expr }

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

parseReturn :: Parser String String Expr
parseReturn = do
  parseStr "..return..("
  expr <- parseExpr
  parseStr ")"
  return expr

parseStatement = do 
  many (parseStr " " <|> parseStr "\n")
  st <- parseL
  parseStr ";"
  many (parseStr " " <|> parseStr "\n")
  return st

parseArgs:: Parser String String [Var]
parseArgs = ((:) <$> parseIdent <*> many (parseStr ", " *> parseIdent)) <|> (pure [])


parseDef :: Parser String String Function
parseDef = do
  many (parseStr " " <|> parseStr "\n")
  parseStr "."
  name <- parseIdent
  parseStr ".("
  args <- parseArgs
  parseStr ")"
  body <- parseSeq
  res <- parseReturn
  many (parseStr " " <|> parseStr "\n")
  return (Function name args body res)

parseProg :: Parser String String Program
parseProg = do
  funcs <- many parseDef
  m <- parseL
  return $ Program funcs m

eval :: LAst -> Configuration -> Maybe Configuration
eval st@(If cond tr fls) config@(Conf subst _ _ d) = do 
  (Conf _ i o _, c) <- evalExpr config cond 
  if (c /= 0) then (eval tr (Conf subst i o d)) else (eval fls (Conf subst i o d))

eval st@(Assign var expr) config@(Conf subst _ _ d) = do 
  ((Conf _ i o _), e) <- evalExpr config expr
  return $ Conf (Map.insert var e subst) i o d

eval st@(Read var) config@(Conf subst (h:i) o d) = return $ Conf (Map.insert var h subst) i o d
eval st@(Read var) config@(Conf subst [] o d) = Nothing

eval st@(Write expr) config@(Conf subst _ _ d) = do 
  (Conf _ i o _, e) <- evalExpr config expr
  return $ Conf subst i (e:o) d 

eval st@(Seq []) config@(Conf subst i o d) = Just config
eval st@(Seq (s:stx)) config@(Conf subst i o d) = do 
  nc <- eval s config
  eval (Seq stx) nc

eval st@(While cond seq) config@(Conf subst _ _ d) = do 
  (Conf _ i o _, expr) <- evalExpr config cond
  if (expr == 0) then return (Conf subst i o d) else do {nc <- eval seq (Conf subst i o d); eval st nc}

eval _ _ = Nothing

evalFunc :: Function -> Configuration -> [Int] -> Maybe (Configuration, Int)
evalFunc (Function _ args (Seq sts) res) (Conf subst i o d) argvals = 
  case eval (Seq (sts ++ [Write res])) (Conf (Map.fromList $ zip args argvals) i o d) of
    Just (Conf _ inp (res:out) d) -> Just (Conf subst inp out d, res)
    otherwise -> Nothing
evalFunc (Function name args st res) conf argvals = evalFunc (Function name args (Seq [st]) res) conf argvals

evalExpr :: Configuration -> Expr -> Maybe (Configuration, Int)
evalExpr conf (Num x) = Just (conf, x)

evalExpr conf@(Conf subst _ _ _) (Ident x) = (,) conf <$> Map.lookup x subst

evalExpr conf (UnaryOp Minus x) = (fmap (*(-1))) <$> evalExpr conf x

evalExpr conf (UnaryOp Not x) = (fmap (fromEnum . (==0))) <$> evalExpr conf x

evalExpr conf (BinOp Plus x y) = do 
  (nconf, r) <- evalExpr conf x
  (fmap $ (+) r) <$> evalExpr nconf y

evalExpr conf (BinOp Mult x y) = do 
  (nconf, r) <- evalExpr conf x
  (fmap $ (*) r) <$> evalExpr nconf y

evalExpr conf (BinOp Minus x y) = do 
  (nconf, r) <- evalExpr conf x
  (fmap $ (-) r) <$> evalExpr nconf y

evalExpr conf (BinOp Div x y) = do 
  (nconf, r) <- evalExpr conf x
  (fmap $ div r) <$> evalExpr nconf y

evalExpr conf (BinOp Pow x y) = do 
  (nconf, r) <- evalExpr conf x 
  (fmap $ (^) r) <$> evalExpr nconf y

evalExpr conf (BinOp Equal x y) = do 
  (nconf, r) <- evalExpr conf x
  (fmap (\r' -> fromEnum (r == r'))) <$> evalExpr nconf y

evalExpr conf (BinOp Nequal x y) = do 
  (nconf, r) <- evalExpr conf x
  (fmap (\r' -> fromEnum (r /= r'))) <$> evalExpr nconf y

evalExpr conf (BinOp Ge x y) = do 
  (nconf, r) <- evalExpr conf x
  (fmap (\r' -> fromEnum (r >= r'))) <$> evalExpr nconf y

evalExpr conf (BinOp Gt x y) = do 
  (nconf, r) <- evalExpr conf x
  (fmap (\r' -> fromEnum (r > r'))) <$> evalExpr nconf y

evalExpr conf (BinOp Le x y) = do 
  (nconf, r) <- evalExpr conf x
  (fmap (\r' -> fromEnum (r <= r'))) <$> evalExpr nconf y

evalExpr conf (BinOp Lt x y) = do 
  (nconf, r) <- evalExpr conf x
  (fmap (\r' -> fromEnum (r < r'))) <$> evalExpr nconf y
  
evalExpr conf (BinOp Or x y) = do 
  (nconf, r) <- evalExpr conf x
  (fmap (\r' -> fromEnum ((r/=0) || (r'/=0)))) <$> evalExpr nconf y

evalExpr conf (BinOp And x y) = do 
  (nconf, r) <- evalExpr conf x
  (fmap (\r' -> fromEnum ((r/=0) && (r'/=0)))) <$> evalExpr nconf y

evalExpr conf@(Conf _ _ _ defs) (FunctionCall f args) = do 
  let helper prev arg = do {(c, arvals) <- prev; (nconf, argval) <- evalExpr c arg; return (nconf, arvals ++ [argval])}
  (conf@(Conf subst _ _ defs'), x) <- foldl helper (Just (conf, [])) args
  f <- Map.lookup f defs
  ((Conf _ i o _), y) <- evalFunc f conf x
  return (Conf subst i o defs', y)


instance Show Function where
  show (Function name args funBody returnExpr) =
    printf "%s(%s) =\n%s\n%s" name (intercalate ", " $ map show args) (unlines $ map (identation 1) $ lines $ show funBody) (identation 1 ("return " ++ show returnExpr))

instance Show Program where
  show (Program defs main) =
    printf "%s\n\n%s" (intercalate "\n\n" $ map show defs) (show main)

instance Eq Function where
  (==) a b = (show a) == (show b)

instance Eq Program where
  (==) a b = (show a) == (show b)

instance Show LAst where
  show =
      go 0
    where
      go n t =
        let makeIdent = identation n in
        case t of
          If cond thn els -> makeIdent $ printf "if %s\n%sthen\n%s\n%selse\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) thn) (makeIdent "") (go (ident n) els)
          While cond body -> makeIdent $ printf "while %s\n%sdo\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) body)
          Assign var expr -> makeIdent $ printf "%s := %s" var (flatShowExpr expr)
          Read var        -> makeIdent $ printf "read %s" var
          Write expr      -> makeIdent $ printf "write %s" (flatShowExpr expr)
          Seq stmts       -> intercalate "\n" $ map (go n) stmts
      flatShowExpr (BinOp op l r) = printf "(%s %s %s)" (flatShowExpr l) (show op) (flatShowExpr r)
      flatShowExpr (UnaryOp op x) = printf "(%s %s)" (show op) (flatShowExpr x)
      flatShowExpr (Ident x) = x
      flatShowExpr (Num n) = show n
      flatShowExpr (FunctionCall name args) = printf "%s(%s)" name (intercalate ", " $ map flatShowExpr args)


ident = (+1)

identation n = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id