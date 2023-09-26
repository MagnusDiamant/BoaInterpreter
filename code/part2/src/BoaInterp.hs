-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'


module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output,
   truthy, operate, apply,
   eval, exec, execute)
  where

import BoaAST
import Control.Monad
import Data.List
--import Data.Maybe

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }



instance Monad Comp where
  return a = Comp (\_ -> (Right a, mempty))
  m >>= f = Comp (\e -> case runComp m e of 
                          (Left e1, s1) -> (Left e1, s1)
                          (Right x1, s1) -> case runComp (f x1) e of
                                            (Left e2, s2) -> (Left e2, s1 <> s2)
                                            (Right x2, s2) -> (Right x2, s1 <> s2))

-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

-- Operations of the monad
abort :: RunError -> Comp a
abort re = Comp (\_ -> (Left re, mempty))

look :: VName -> Comp Value
look v = Comp (\e -> case find (\(x, _) -> x == (v)) e of Just (_, z) -> (Right z, mempty)
                                                          Nothing -> (Left (EBadVar v), mempty))

withBinding :: VName -> Value -> Comp a -> Comp a
withBinding x v m = Comp (\e -> runComp m ([(x, v)] <> e)) 

output :: String -> Comp ()
output s = Comp (\_ -> (Right (), [s])) 

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy e = if elem e [(IntVal 0), (NoneVal), (FalseVal), (ListVal []), (StringVal "")] then False else True

operate :: Op -> Value -> Value -> Either String Value
operate Plus (IntVal v1) (IntVal v2) = Right (IntVal (v1 + v2))
operate Minus (IntVal v1) (IntVal v2) = Right (IntVal (v1 - v2))
operate Times (IntVal v1) (IntVal v2) = Right (IntVal (v1 * v2))
operate Div (IntVal _) (IntVal 0) = Left "Division by 0"
operate Div (IntVal v1) (IntVal v2) = Right (IntVal (v1 `div` v2))
operate Mod (IntVal _) (IntVal 0) = Left "Modulo by 0"
operate Mod (IntVal v1) (IntVal v2) = Right (IntVal (v1 `mod` v2))
operate Eq (v1) (v2) = if v1 == v2 then Right TrueVal else Right FalseVal --Hvordan sikrer vi os, at det er samme type
--operate Eq (StringVal v1) (StringVal v2) = if v1 == v2 then Right TrueVal else Right FalseVal --Hvordan sikrer vi os, at det er samme type
--operate Eq (ListVal v1) (ListVal v2) = if v1 == v2 then Right TrueVal else Right FalseVal --Hvordan sikrer vi os, at det er samme type
operate Less (IntVal v1) (IntVal v2) = if v1 < v2 then Right TrueVal else Right FalseVal
operate Greater (IntVal v1) (IntVal v2) = if v1 > v2 then Right TrueVal else Right FalseVal
operate In (v1) (ListVal v2) = if elem v1 v2 == True then Right TrueVal else Right FalseVal -- Hvis de ikke har samme type?
operate _ _ _ = Left "Unknown error - Most likely the wrong types for that specific operation"


printHelper :: [Value] -> String
printHelper [] = ""
printHelper (IntVal h : t) = if length t == 0 then (show h) ++ (printHelper t) else (show h) ++ " " ++ (printHelper t)
printHelper (StringVal h : t) = if length t == 0 then h ++ (printHelper t) else h ++ " " ++ (printHelper t)
printHelper (NoneVal : t) = if length t == 0 then "None" ++ (printHelper t) else "None " ++ (printHelper t)
printHelper (TrueVal : t) = if length t == 0 then "True" ++ (printHelper t) else "True " ++ (printHelper t)
printHelper (FalseVal : t) = if length t == 0 then "False" ++ (printHelper t) else "False " ++ (printHelper t)
printHelper ([ListVal []]) = "[]"
printHelper (ListVal h : t) = if length t == 0 then "[" ++ (printHelper2 h) ++ "]" else "[" ++ (printHelper2 h) ++ "], " ++ (printHelper t)


printHelper2 :: [Value] -> String
printHelper2 [] = ""
printHelper2 (IntVal h : t) = if length t == 0 then (show h) ++ (printHelper2 t) else (show h) ++ ", " ++ (printHelper2 t)
printHelper2 (StringVal h : t) = if length t == 0 then h ++ (printHelper2 t) else h ++ ", " ++ (printHelper2 t)
printHelper2 (NoneVal : t) = if length t == 0 then "None" ++ (printHelper2 t) else "None, " ++ (printHelper2 t)
printHelper2 (TrueVal : t) = if length t == 0 then "True" ++ (printHelper2 t) else "True, " ++ (printHelper2 t)
printHelper2 (FalseVal : t) = if length t == 0 then "False" ++ (printHelper2 t) else "False, " ++ (printHelper2 t)
printHelper2 ([ListVal []]) = "[]"
printHelper2 (ListVal h : t) = if length t == 0 then "[" ++ (printHelper2 h) ++ "]" else "[" ++ (printHelper2 h) ++ "], " ++ (printHelper2 t)


apply :: FName -> [Value] -> Comp Value
apply "range" [(IntVal _), (IntVal _), (IntVal 0)] = abort (EBadArg "Invalid input") 
apply "range" [(IntVal n1), (IntVal n2), (IntVal n3)] 
                        | (n3 < 0) && (n1 > n2) = return (ListVal (map IntVal [n1, (n1+n3)..(n2+1)])) 
                        | (n3 > 0) && (n1 < n2) = return (ListVal (map IntVal [n1, (n1+n3)..(n2-1)]))
                        | otherwise = return (ListVal [])
apply "range" [(IntVal n1), (IntVal n2)] 
                        | (n1 < n2) = return (ListVal (map IntVal [n1, 1..n2-1]))
                        | otherwise = return (ListVal [])
apply "range" [(IntVal n1)] 
                        | (n1 > 0) = return (ListVal (map IntVal [0, 1..n1-1]))
                        | otherwise = return (ListVal [])
apply "print" v = do 
  output (printHelper v)
  return NoneVal
apply "range" _ = abort (EBadArg "Bad argument")
apply x _ = abort (EBadFun x) 

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval (Const v) = return v
eval (Var x) = look x
eval (Oper o e1 e2) = do
  evaluated1 <- eval e1 
  evaluated2 <- eval e2 
  case operate o evaluated1 evaluated2 of
    Left err -> abort (EBadArg err)
    Right evaluated3 -> return evaluated3
eval (Not e1) = do
  evaluated1 <- eval e1
  if elem evaluated1 [(IntVal 0), (FalseVal), (NoneVal), (ListVal [])] 
    then  return TrueVal
    else return FalseVal
eval (Call f e) = do
  evaluated1 <- mapM eval e
  apply f evaluated1
eval (List []) = abort (EBadArg "Empty list")
eval (List e) = do
  evaluated1 <- mapM eval e
  return (ListVal evaluated1)
eval (Compr _ _) = abort (EBadArg "error") 
  --range <- eval e1
  --[e | x <- range]
  --withBinding x evaluated1 (eval (Compr e t))
  --evaluated1 <- eval e1
  --evaluated2 <- eval e2
  --[evaluated1 | x <- evaluated2]  
--eval (List (e : t)) = (eval e) <> (eval (List t))
eval _ = abort (EBadArg "Test fejl")


exec :: Program -> Comp ()
exec [] = return ()
exec (h : t) = case h of
                  SDef v e -> do
                    v1 <- eval e
                    withBinding v v1 (exec t)
                  SExp e -> do
                    eval e
                    exec t
 
 
execute :: Program -> ([String], Maybe RunError)
execute p = case runComp (exec p) mempty of
  (Left (err), s) -> (s, Just err)
  (Right (_), s) -> (s, Nothing)


