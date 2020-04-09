module Test.LLang where

import           AST                 (AST (..), Operator (..))
import           Combinators         (Parser (..), Result (..), runParser)
import           LLang
import           Test.Tasty.HUnit    (Assertion, (@?=), assertBool)

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
