module Regexp where

import Prelude hiding (seq)

data Regexp = Empty
            | Epsilon
            | Char Char
            | Seq Regexp Regexp
            | Alt Regexp Regexp
            | Star Regexp
            deriving (Show, Eq, Ord)

match :: Regexp -> String -> Bool
match r s = nullable (foldl (flip derivative) r s)

derivative :: Char -> Regexp -> Regexp
derivative a reg = case reg of
    Empty -> Empty
    Epsilon -> Empty
    Char b -> if (a == b) then Epsilon else Empty
    Seq r s -> if (nullable r) then (Alt (Seq (derivative a r) s) (derivative a s)) else (Seq (derivative a r) s)
    Alt r s -> Alt (derivative a s) (derivative a r)
    Star r -> Seq (derivative a r) reg

nullable :: Regexp -> Bool
nullable reg = case reg of
    Empty -> False
    Epsilon -> True
    Char b -> False
    Seq r s -> (nullable r) && (nullable s)
    Alt r s -> (nullable r) || (nullable s)
    Star r -> True