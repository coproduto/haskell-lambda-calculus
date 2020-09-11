module Lib
    ( someFunc
    ) where

data Term = Var String Int
          | Abs Term
          | App Term Term
          deriving Show

evalTerm :: [Term] -> Term -> Term
evalTerm env (Var name index)
  | index < 0 = (Var name index)
  | otherwise = env !! index
evalTerm env (Abs body)  = Abs (evalTerm env body)
evalTerm env (App (Abs body) term) =
  let arg = evalTerm env term in
    evalTerm (arg:env) body
evalTerm env (App t1 t2) = App (evalTerm env t1) (evalTerm env t2)

eval :: Term -> Term
eval = evalTerm []

main :: IO ()
main = print $ eval (App (Abs (Var "x" 0)) (Var "x" (-1)))
