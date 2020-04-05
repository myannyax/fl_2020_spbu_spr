module Test.LLang where

import           AST                 (AST (..), Operator (..))
import           Combinators         (Parser (..), Result (..), runParser)
import           LLang
import           Test.Tasty.HUnit    (Assertion, (@?=), assertBool)
import qualified Data.Map         as Map
import           LLang            (Configuration (..), LAst (..), eval,
                                   initialConf)
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))

isFailure (Failure _) = True
isFailure  _          = False

unit_parseIf :: Assertion
unit_parseIf = do 
    runParser parseIf "if(x==0){x:=0;}else{write(0);}" @?= Success "" (If {
       cond = BinOp Equal (Ident "x") (Num 0),
       thn  = Seq [Assign "x" (Num 0)],
       els  = Seq [Write (Num 0)]})

    assertBool "" $ isFailure $ runParser parseIf "If(x==0){x:=0;}else{write(0);}"
    assertBool "" $ isFailure $ runParser parseIf "if(x==0) {x:=0;}else{write(0);}"
    assertBool "" $ isFailure $ runParser parseIf "if(x==0){x:=0;}{write(0);}"

unit_parseWhile = do 
    runParser parseWhile "while(x==0){x:=0;}" @?= Success "" (While {
       cond = BinOp Equal (Ident "x") (Num 0),
       body  = Seq [Assign "x" (Num 0)]})
    assertBool "" $ isFailure $ runParser parseWhile "While(x==0){x:=0;}"
    assertBool "" $ isFailure $ runParser parseWhile "while (x==0){x:=0;}"
    assertBool "" $ isFailure $ runParser parseWhile "while(x==0)"

unit_parseAssign = do
    runParser parseAssign "x:=0" @?= Success "" (Assign "x" (Num 0))
    assertBool "" $ isFailure $ runParser parseAssign "x=0"

unit_parseRead = do
    runParser parseRead "read(x)" @?= Success "" (Read "x")

    assertBool "" $ isFailure $ runParser parseRead "Read(x)"
    assertBool "" $ isFailure $ runParser parseRead "read (x)"

unit_parseWrite = do
    runParser parseWrite "write(0)" @?= Success "" (Write (Num 0))

    assertBool "" $ isFailure $ runParser parseWrite "Write(0)"

unit_parseSeq = do
    runParser parseSeq "{write(0);read(x); \n \n \n  \n write(0);}" @?= Success "" (Seq [(Write (Num 0)), (Read "x"), (Write (Num 0))])
    runParser parseSeq "{\n\n\n \n\n\n write(0);read(x); \n \n \n  \n write(0);\n\n\n\n }" @?= Success "" (Seq [(Write (Num 0)), (Read "x"), (Write (Num 0))])

    runParser parseSeq "{}"  @?= Success "" (Seq [])
    runParser parseSeq "{ }" @?= Success "" (Seq [])
    assertBool "" $ isFailure $ runParser parseSeq "{write(0);read(x); \n \n \n  \n write(0)}"
    assertBool "" $ isFailure $ runParser parseSeq "\n{write(0);read(x); \n \n \n  \n write(0);}"

    assertBool "" $ isFailure $ runParser parseSeq "{write(0);rread(x); \n \n \n  \n write(0);}"
    assertBool "" $ isFailure $ runParser parseSeq "{write(0);;read(x); \n \n \n  \n write(0);}"
    assertBool "" $ isFailure $ runParser parseSeq "write(0);read(x); \n \n \n  \n write(0);"

-- read x;
-- if (x > 13)
-- then { write x }
-- else {
--     while (x < 42) {
--       x := x * 7;
--       write (x);
--     }
-- }
stmt1 :: LAst
stmt1 =
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

unit_stmt1 :: Assertion
unit_stmt1 = do
  let xIs n = Map.fromList [("X", n)]
  eval stmt1 (initialConf [1]) @?= Just (Conf (xIs 49) [] [49, 7])
  eval stmt1 (initialConf [10]) @?= Just (Conf (xIs 70) [] [70])
  eval stmt1 (initialConf [42]) @?= Just (Conf (xIs 42) [] [42])


-- read x;
-- if (x)
-- then {
--   while (x) {
--     x := x - 2;
--     write (x);
--   }
-- else {}
stmt2 :: LAst
stmt2 =
  Seq
    [ Read "x"
    , If (Ident "x")
         (While (Ident "x")
                (Seq
                   [ (Assign "x" (BinOp Minus (Ident "x") (Num 2)))
                   , (Write (Ident "x"))
                   ]
                )
         )
         (Seq [])
    ]

unit_stmt2 :: Assertion
unit_stmt2 = do
  let xIs n = Map.fromList [("x", n)]
  eval stmt2 (initialConf [0]) @?= Just (Conf (xIs 0) [] [])
  eval stmt2 (initialConf [2]) @?= Just (Conf (xIs 0) [] [0])
  eval stmt2 (initialConf [42]) @?= Just (Conf (xIs 0) [] (filter even [0 .. 40]))

-- read x;
-- read y;
-- write (x == y);
stmt3 :: LAst
stmt3 =
  Seq
    [ Read "x"
    , Read "y"
    , Write (BinOp Equal (Ident "x") ((Ident "y")))
    ]

unit_stmt3 :: Assertion
unit_stmt3 = do
  let subst x y = Map.fromList [("x", x), ("y", y) ]
  eval stmt3 (initialConf [0, 2]) @?= Just (Conf (subst 0 2) [] [0])
  eval stmt3 (initialConf [2, 2]) @?= Just (Conf (subst 2 2) [] [1])
  eval stmt3 (initialConf [42]) @?= Nothing

-- read n;
-- if (n == 1 || n == 2)
-- then {
--   write 1;
-- }
-- else {
--   i := 2;
--   cur := 1
--   prev := 1
--   while (i < n) {
--     temp := cur + prev;
--     prev := cur;
--     cur := temp;
--     i := i + 1;
--   }
--   write (cur);
-- }
stmt4 :: LAst
stmt4 =
  Seq
    [ Read "n"
    , If (BinOp Or (BinOp Equal (Ident "n") (Num 1)) (BinOp Equal (Ident "n") (Num 2)))
         (Write (Num 1))
         (Seq
            [ Assign "i" (Num 2)
            , Assign "cur" (Num 1)
            , Assign "prev" (Num 1)
            , While (BinOp Lt (Ident "i") (Ident "n"))
                     (Seq
                        [ Assign "temp" (BinOp Plus (Ident "cur") (Ident "prev"))
                        , Assign "prev" (Ident "cur")
                        , Assign "cur" (Ident "temp")
                        , Assign "i" (BinOp Plus (Ident "i") (Num 1))
                        ]
                     )
            , Write (Ident "cur")
            ]
         )
    ]

unit_stmt4 :: Assertion
unit_stmt4 = do
  let subst n i cur prev temp = Map.fromList [("n", n), ("i", i), ("cur", cur), ("prev", prev), ("temp", temp)]
  let subst' n = Map.fromList [("n", n)]
  eval stmt4 (initialConf [1]) @?= Just (Conf (subst' 1) [] [1])
  eval stmt4 (initialConf [2]) @?= Just (Conf (subst' 2) [] [1])
  eval stmt4 (initialConf [10]) @?= Just (Conf (subst 10 10 55 34 55) [] [55] )
  eval stmt4 (initialConf []) @?= Nothing
