module Test.LLang where

import           AST
import           Combinators      (Parser (..), Result (..), runParser, toStream, InputStream (..),
                                   Position (..), ErrorMsg (..))
import qualified Data.Map         as Map
import           Debug.Trace      (trace)
import           LLang
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import           Text.Printf      (printf)

isFailure (Failure _) = True
isFailure  _          = False

initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input [] Map.empty


check :: (Eq r, Show r) => Parser String String r -> String -> r -> Assertion
check parser prog result = do
  case runParser parser prog of
       Success (InputStream "" _) _ -> 1 @?= 1
       otherwise -> prog @?= "parseError"

checkPos pos prog lasts = case runParser parseProg prog of
      Success (InputStream lasts' pos') _ -> (pos', lasts') @?= (pos, lasts)
      otherwise -> prog @?= "parseError" 

unit_parseIf :: Assertion
unit_parseIf = do 
    check parseIf "if(x==0){x:=0;}else{write(0);}" (If {
       cond = BinOp Equal (Ident "x") (Num 0),
       thn  = Seq [Assign "x" (Num 0)],
       els  = Seq [Write (Num 0)]})

    assertBool "" $ isFailure $ runParser parseIf "If(x==0){x:=0;}else{write(0);}"
    assertBool "" $ isFailure $ runParser parseIf "if(x==0) {x:=0;}else{write(0);}"
    assertBool "" $ isFailure $ runParser parseIf "if(x==0){x:=0;}{write(0);}"

unit_parseWhile = do 
    check parseWhile "while(x==0){x:=0;}" (While {
       cond = BinOp Equal (Ident "x") (Num 0),
       body  = Seq [Assign "x" (Num 0)]})
    assertBool "" $ isFailure $ runParser parseWhile "While(x==0){x:=0;}"
    assertBool "" $ isFailure $ runParser parseWhile "while (x==0){x:=0;}"
    assertBool "" $ isFailure $ runParser parseWhile "while(x==0)"

unit_parseAssign = do
    check parseAssign "x:=0" (Assign "x" (Num 0))
    assertBool "" $ isFailure $ runParser parseAssign "x=0"

unit_parseRead = do
    check parseRead "read(x)" (Read "x")

    assertBool "" $ isFailure $ runParser parseRead "Read(x)"
    assertBool "" $ isFailure $ runParser parseRead "read (x)"

unit_parseWrite = do
    check parseWrite "write(0)" (Write (Num 0))

    assertBool "" $ isFailure $ runParser parseWrite "Write(0)"

unit_parseSeq = do
    check parseSeq "{write(0);read(x); \n \n \n  \n write(0);}" (Seq [(Write (Num 0)), (Read "x"), (Write (Num 0))])
    check parseSeq "{\n\n\n \n\n\n write(0);read(x); \n \n \n  \n write(0);\n\n\n\n }" (Seq [(Write (Num 0)), (Read "x"), (Write (Num 0))])

    check parseSeq "{}" (Seq [])
    check parseSeq "{ }" (Seq [])
    assertBool "" $ isFailure $ runParser parseSeq "{write(0);read(x); \n \n \n  \n write(0)}"
    assertBool "" $ isFailure $ runParser parseSeq "\n{write(0);read(x); \n \n \n  \n write(0);}"

    assertBool "" $ isFailure $ runParser parseSeq "{write(0);rread(x); \n \n \n  \n write(0);}"
    assertBool "" $ isFailure $ runParser parseSeq "{write(0);;read(x); \n \n \n  \n write(0);}"
    assertBool "" $ isFailure $ runParser parseSeq "write(0);read(x); \n \n \n  \n write(0);"


stmt1 :: LAst
stmt1 =
  Seq
    [ Read "x"
    , If (BinOp Gt (Ident "x") (Num 13))
         (Seq [(Write (Ident "x"))])
         (Seq [(While (BinOp Lt (Ident "x") (Num 42))
                (Seq [ Assign "x"
                        (BinOp Mult (Ident "x") (Num 7))
                     , Write (Ident "x")
                     ]
                )
         )])
    ]

unit_stmt1 :: Assertion
unit_stmt1 = do
  let xIs n = Map.fromList [("x", n)]
  eval stmt1 (initialConf [1]) @?= Just (Conf (xIs 49) [] [49, 7] Map.empty)
  eval stmt1 (initialConf [10]) @?= Just (Conf (xIs 70) [] [70] Map.empty)
  eval stmt1 (initialConf [42]) @?= Just (Conf (xIs 42) [] [42] Map.empty)


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
         (Seq [(While (Ident "x")
                (Seq
                   [ (Assign "x" (BinOp Minus (Ident "x") (Num 2)))
                   , (Write (Ident "x"))
                   ]
                )
         )])
         (Seq [])
    ]

unit_stmt2 :: Assertion
unit_stmt2 = do
  let xIs n = Map.fromList [("x", n)]
  eval stmt2 (initialConf [0]) @?= Just (Conf (xIs 0) [] [] Map.empty)
  eval stmt2 (initialConf [2]) @?= Just (Conf (xIs 0) [] [0] Map.empty)
  eval stmt2 (initialConf [42]) @?= Just (Conf (xIs 0) [] (filter even [0 .. 40]) Map.empty)

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
  eval stmt3 (initialConf [0, 2]) @?= Just (Conf (subst 0 2) [] [0] Map.empty)
  eval stmt3 (initialConf [2, 2]) @?= Just (Conf (subst 2 2) [] [1] Map.empty)
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
         (Seq [(Write (Num 1))])
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
  eval stmt4 (initialConf [1]) @?= Just (Conf (subst' 1) [] [1] Map.empty)
  eval stmt4 (initialConf [2]) @?= Just (Conf (subst' 2) [] [1] Map.empty)
  eval stmt4 (initialConf [10]) @?= Just (Conf (subst 10 10 55 34 55) [] [55] Map.empty)
  eval stmt4 (initialConf []) @?= Nothing


unit_progPosition :: Assertion
unit_progPosition = do
    checkPos (Position 0 30) "if(x==0){x:=0;}else{write(0);}" ""
    checkPos (Position 4 1) "if(x==0){\nx:=0;\n}else{\n    write(0);\n}" ""