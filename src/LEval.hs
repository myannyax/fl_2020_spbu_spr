module LEval where

import LLang (Program (..), Configuration (..), eval, Function(..), parseProg)
import Combinators (InputStream (..), Result (..), runParser)
import qualified Data.Map    as Map

evalProg :: Program -> [Int] -> Maybe Configuration
evalProg (Program fs main) i = 
    eval main (Conf Map.empty i [] (Map.fromList (fmap (\f@(Function name _ _ _) -> (name, f)) fs)))

parseAndEvalProg :: String -> [Int] -> Maybe Configuration
parseAndEvalProg str i = case runParser parseProg str of
    Success (InputStream s pos) prog -> evalProg prog i