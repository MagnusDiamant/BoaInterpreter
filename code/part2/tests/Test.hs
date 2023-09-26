-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import BoaAST
import BoaInterp

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

e :: Env
e = [("X", IntVal 3), ("Y", IntVal 4), ("Z", TrueVal)]

tests :: TestTree
tests = testGroup "Stubby tests"
  [ 
   testCase "Oper Plus" $
      operate Plus (IntVal 2) (IntVal 2) @?= Right (IntVal 4)
  , testCase "Oper Minus" $
      operate Minus (IntVal 10) (IntVal 5) @?= Right (IntVal 5)
  , testCase "Oper Times" $
      operate Times (IntVal 2) (IntVal 3) @?= Right (IntVal 6)
  , testCase "Oper Div Right" $
      operate Div (IntVal 10) (IntVal 5) @?= Right (IntVal 2)
  , testCase "Oper Div - Div 0" $
      operate Div (IntVal 10) (IntVal 0) @?= Left "Division by 0"
  , testCase "Oper Mod - Right" $
      operate Mod (IntVal 10) (IntVal 2) @?= Right (IntVal 0)
  , testCase "Oper Mod - Mod 0" $
      operate Mod (IntVal 10) (IntVal 0) @?= Left "Modulo by 0"
  , testCase "Oper Eq - True" $
      operate Eq (IntVal 10) (IntVal 10) @?= Right TrueVal
  , testCase "Oper Eq - False" $
      operate Eq (IntVal 10) (IntVal 9) @?= Right FalseVal
  , testCase "Oper Less - True" $
      operate Less (IntVal 6) (IntVal 7) @?= Right TrueVal
  , testCase "Oper Less - False" $
      operate Less (IntVal 7) (IntVal 6) @?= Right FalseVal
  , testCase "Oper Greater- True" $
      operate Greater (IntVal 10) (IntVal 9) @?= Right TrueVal
  , testCase "Oper Greater - False" $
      operate Greater (IntVal 9) (IntVal 10) @?= Right FalseVal
  , testCase "Oper In - True" $
      operate In (IntVal 6) (ListVal [(IntVal 3), (IntVal 6)]) @?= Right TrueVal
  , testCase "Oper In - False" $
      operate In (IntVal 6) (ListVal [(IntVal 4), (IntVal 5)]) @?= Right FalseVal
  , testCase "truthy - True" $
      truthy TrueVal @?= True
  , testCase "truthy - False" $
      truthy FalseVal @?= False
  , testCase "look - Right" $
      runComp (look "X") [("X", IntVal 3), ("Y", IntVal 4), ("Z", TrueVal)] @?= (Right (IntVal 3),[])
  , testCase "look - Left" $
      runComp (look "A") [("X", IntVal 3), ("Y", IntVal 4), ("Z", TrueVal)] @?= (Left (EBadVar "A"),[])
  , testCase "output" $
      runComp (output "test") e @?= (Right (),["test"])
  , testCase "apply - n3 = 0" $
      runComp (apply "range" [(IntVal 4), (IntVal 6), (IntVal 0)]) e @?= (Left (EBadArg "Invalid input"),[])
  , testCase "apply - n1<n2 and n3 = 1" $
      runComp (apply "range" [(IntVal 4), (IntVal 5), (IntVal 1)]) e @?= (Right (ListVal [IntVal 4]),[])
  , testCase "apply - n1<n2 and n3 = 2" $
      runComp (apply "range" [(IntVal 11), (IntVal 20), (IntVal 2)]) e @?= (Right (ListVal [IntVal 11,IntVal 13,IntVal 15,IntVal 17,IntVal 19]),[])
  , testCase "apply - n1>n2 and n3 = 2" $
      runComp (apply "range" [(IntVal 4), (IntVal 3), (IntVal 2)]) e @?= (Right (ListVal []),[])
  , testCase "apply - n1<n2 and n3 = -1" $
      runComp (apply "range" [(IntVal 2), (IntVal 6), (IntVal (-1))]) e @?= (Right (ListVal []),[])
  , testCase "eval - Const" $
      runComp (eval (Const (IntVal 4))) e @?= (Right (IntVal 4),[])
  , testCase "eval - look" $
      runComp (eval (Var "Z")) e @?= (Right TrueVal,[])
  , testCase "eval - Oper Plus" $
      runComp (eval (Oper Plus (Const (IntVal 6)) (Const (IntVal 6)))) e @?= (Right (IntVal 12),[])
  , testCase "eval - Oper Times" $
      runComp (eval (Oper Times (Const (IntVal 4)) (Const (IntVal 4)))) e @?= (Right (IntVal 16),[])
  , testCase "eval - Not []" $
      runComp (eval (Not (Const (ListVal [])))) e @?= (Right TrueVal,[])
  , testCase "eval - Not IntVal 1" $
      runComp (eval (Not (Const (IntVal 6)))) e @?= (Right FalseVal,[])
  , testCase "eval - Call print IntVal 2" $
      runComp (eval (Call "print" [(Const (IntVal 9))])) e @?= (Right NoneVal,["9"])
  , testCase "eval - Call print list" $
      runComp (eval (Call "print" [(Const (IntVal 1)), (Const (IntVal 2)),(Const (IntVal 3))])) e @?= (Right NoneVal,["1 2 3"])
  , testCase "eval - Call range list" $
      runComp (eval (Call "range" [(Const (IntVal 4)), (Const (IntVal 8)),(Const (IntVal 2))])) e @?= (Right (ListVal [IntVal 4,IntVal 6]),[])
  , testCase "eval - List" $
      runComp (eval (List [(Const (IntVal 5)), (Const (IntVal 6))])) e @?= (Right (ListVal [IntVal 5,IntVal 6]),[])
  ]

