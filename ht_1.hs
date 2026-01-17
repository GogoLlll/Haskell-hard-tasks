module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data SExpr
  = Atom String
  | Number Integer
  | List   [SExpr]
  | Lambda [String] SExpr Env   
  deriving (Eq)

instance Show SExpr where
  show = pp
type Env = Map String SExpr

pp :: SExpr -> String
pp (Atom a)    = a
pp (Number n)  = show n
pp (List xs)   = "(" ++ unwords (map pp xs) ++ ")"
pp (Lambda args body _) = "(lambda (" ++ unwords args ++ ") " ++ pp body ++ ")"

eval :: Env -> SExpr -> SExpr
eval _   n@(Number _) = n

eval env (Atom name) =
  fromMaybe (error $ "Unknown variable: " ++ name)
            (Map.lookup name env)

eval env (List (Atom "if" : cond : tExpr : eExpr : [])) =
  case eval env cond of
    Number 0 -> eval env eExpr
    _        -> eval env tExpr

eval env (List (Atom "define" : Atom name : value : [])) =
  eval env value 

eval env (List (Atom "lambda" : List args : body : [])) =
  let argNames = [name | Atom name <- args]
  in Lambda argNames body env

eval env (List (fn : args)) =
  case eval env fn of
    Lambda argNames body closureEnv ->
      let argVals = map (eval env) args
          extendedEnv = Map.union (Map.fromList (zip argNames argVals)) closureEnv
      in eval extendedEnv body

    otherFunc -> applyBuiltin otherFunc (map (eval env) args)

eval _ expr = error $ "Incorrect expression: " ++ show expr

applyBuiltin :: SExpr -> [SExpr] -> SExpr
applyBuiltin (Atom "+") [Number a, Number b] = Number (a + b)
applyBuiltin (Atom "-") [Number a, Number b] = Number (a - b)
applyBuiltin (Atom "*") [Number a, Number b] = Number (a * b)
applyBuiltin (Atom ">") [Number a, Number b] = Number (if a > b then 1 else 0)
applyBuiltin f _ = error $ "Unknown function: " ++ show f

initialEnv :: Env
initialEnv = Map.fromList
  [ ("+", Atom "+")
  , ("-", Atom "-")
  , ("*", Atom "*")
  , (">", Atom ">")
  ]

examples :: [(String, SExpr)]
examples =
  [ ("(+ 2 3)", List [Atom "+", Number 2, Number 3])
  , ("(* 2 (+ 3 4))", List [Atom "*", Number 2, List [Atom "+", Number 3, Number 4]])
  , ("(if (> 3 5) 10 20)", List [Atom "if", List [Atom ">", Number 3, Number 5], Number 10, Number 20])
  , ("((lambda (x y) (+ x y)) 5 6)", List [List [Atom "lambda", List [Atom "x", Atom "y"], List [Atom "+", Atom "x", Atom "y"]], Number 5, Number 6])
  ]

main :: IO ()
main = do
  mapM_ run examples
  where
    run (name, expr) = do
      putStrLn $ "\nExpr: " ++ name
      putStrLn $ "=> " ++ show (eval initialEnv expr)