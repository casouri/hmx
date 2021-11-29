module Lang where

import Text.Parsec hiding (State, between)
import Text.Parsec.String
import qualified Data.Map as M
import Control.Monad.Trans.State
import Control.Monad
import Debug.Trace (trace)
import Control.Monad.Except
import System.IO
import Text.Printf

data Expression
   = ExprSymbol Symbol
   | ExprInt Int
   | ExprString String
   | ExprBool Bool
   | ExprList [Expression]
   deriving (Eq, Show)

type Symbol = String

type Obarray = [M.Map Symbol Expression]

type Buffer = (String, Int)

type Error = (Expression, String)

type StateExcept e s a = StateT s (Except e) a
type ExpressionS = StateExcept (Expression, String) Obarray Expression

-- Print

prin1 :: Expression -> String
prin1 (ExprSymbol sym) = sym
prin1 (ExprInt n) = show n
prin1 (ExprBool True) = "true"
prin1 (ExprString str) = "\"" ++ str ++ "\""
prin1 (ExprBool False) = "false"
prin1 (ExprList lst) = "(" ++ unwords (map prin1 lst) ++ ")"

-- Obarray

nil :: Expression
nil = ExprList []

olookup :: Symbol -> Obarray -> ExpressionS
olookup sym [] = undefined
olookup sym [frame] = case M.lookup sym frame of
  Nothing -> throwError (ExprSymbol sym, "Symbol not found")
  Just a -> return a
olookup sym (this:rest) = case M.lookup sym this of
  Nothing -> olookup sym rest
  Just val -> return val

oset :: Symbol -> Expression -> Obarray -> Obarray
oset _ _ [] = undefined
oset sym val [frame] = [M.alter (\x-> Just val) sym frame]
oset sym val (this : rest) =
  case M.lookup sym this of
    Nothing -> this : oset sym val rest
    Just _ -> M.adjust (const val) sym this : rest

initialObarray :: Obarray
initialObarray = [M.empty]

--- Error

throwCurrentForm :: String -> ExpressionS
throwCurrentForm msg = do obarray <- get
                          form <- olookup "__current-form" obarray
                          throwError (form, msg)

genericError :: ExpressionS
genericError = throwCurrentForm "Malformed expression"

--- Primitive bop
  
primAdd (ExprInt a) (ExprInt b) = return $ ExprInt (a + b)
primAdd _ _ = genericError
primSub (ExprInt a) (ExprInt b) = return $ ExprInt (a - b)
primSub _ _ = genericError
primMul (ExprInt a) (ExprInt b) = return $ ExprInt (a * b)
primMul _ _ = genericError
primDiv (ExprInt a) (ExprInt 0) = throwCurrentForm "Division by zero"
primDiv (ExprInt a) (ExprInt b) = return $ ExprInt (div a b)
primDiv _ _ = genericError
primEq a b = return $ ExprBool (a == b)
primGt (ExprInt a) (ExprInt b) = return $ ExprBool (a > b)
primGt _ _ = genericError
primLt (ExprInt a) (ExprInt b) = return $ ExprBool (a < b)
primLt _ _ = genericError
primGe (ExprInt a) (ExprInt b) = return $ ExprBool (a >= b)
primGe _ _ = genericError
primLe (ExprInt a) (ExprInt b) = return $ ExprBool (a <= b)
primLe _ _ = genericError
primAnd (ExprBool a) (ExprBool b) = return $ ExprBool (a && b)
primAnd _ _ = genericError
primOr (ExprBool a) (ExprBool b) = return $ ExprBool (a || b)
primOr _ _ = genericError
primMax (ExprInt a) (ExprInt b) = return $ ExprInt (max a b)
primMax _ _ = genericError
primMin (ExprInt a) (ExprInt b) = return $ ExprInt (min a b)
primMin _ _ = genericError
primNot (ExprBool a) = return $ ExprBool (not a)
primNot _ = genericError

bopPrimitives :: M.Map Symbol
  (Expression -> Expression -> ExpressionS)
bopPrimitives = M.fromList [("+", primAdd),
                            ("-", primSub),
                            ("*", primMul),
                            ("/", primDiv),
                            ("=", primEq),
                            (">", primGt),
                            ("<", primLt),
                            (">=", primGe),
                            ("<=", primLe),
                            ("and", primAnd),
                            ("or", primOr),
                            ("max", primMax),
                            ("min", primMin)]

primitiveFunctions :: [Symbol]
primitiveFunctions =
  ["not", "car", "cdr", "list", "listp", "symbolp",
   "take", "drop", "append", "length"]

primitives :: [Symbol]
primitives = ["if", "let", "setq", "progn", "quote", "intern",
              "funcall"]
             ++ primitiveFunctions ++ M.keys bopPrimitives


--- Buffer
  
insert :: String -> Int -> String -> String
insert body pos txt = take pos body ++ txt ++ drop pos body

delete :: String -> Int -> Int -> String
delete body pos len = take (pos-len) body ++ drop pos body

--- Parse

intP :: Parser Expression
intP = fmap (ExprInt . read) (spaces >> many1 digit)

symbolP :: Parser Expression
symbolP = fmap ExprSymbol (spaces >> many1
                           (letter <|> digit
                            <|> oneOf "!@#$%^&*_+-=<>?/|~`"))

stringP :: Parser Expression
stringP = do spaces
             char '"'
             str <- many (noneOf "\"")
             char '"'
             return $ ExprString str

boolP :: Parser Expression
boolP = try (spaces >> string "true" >> return (ExprBool True))
        <|> (spaces >> string "false" >> return (ExprBool False))

qexprP = do spaces
            char '\''
            exp <- exprP'
            return $ ExprList [ExprSymbol "quote", exp]

exprP' :: Parser Expression
exprP' = try intP
         <|> try boolP
         <|> try symbolP
         <|> try stringP
         <|> try qexprP
         <|> do spaces
                char '('
                stuff <- try multiExprP <|> return []
                spaces
                char ')'
                return $ ExprList stuff

exprP = do exp <- exprP'
           spaces
           return exp

multiExprP :: Parser [Expression]
multiExprP = many1 exprP

parseString :: Parser a -> String -> Either ParseError a
parseString p s = runParser p () "REPL" s

--- Eval

evalExpr :: Expression -> ExpressionS

evalExpr v@(ExprList []) = return v
evalExpr v@(ExprList ((ExprSymbol "lambda") : _)) = return v
evalExpr v@(ExprList (fn : args)) =
  do fn <- evalExpr fn
     obarray <- get
     put (oset "__current-form" v obarray)
     evalList fn args
evalExpr v@(ExprInt _) = return v
evalExpr v@(ExprBool _) = return v
evalExpr v@(ExprString _) = return v
evalExpr v@(ExprSymbol sym) =
  if elem sym primitives
  then return v
  else do obarray <- get
          olookup sym obarray

evalMultiExpr :: [Expression] -> ExpressionS
evalMultiExpr exprs = fmap last (mapM evalExpr exprs)

evalList :: Expression -> [Expression] -> ExpressionS

evalList (ExprSymbol "let") ((ExprList bindings) : body) =
  do obarray <- get
     bds <- let bindingToList (ExprList [ExprSymbol sym, val]) =
                  do val <- evalExpr val
                     return (sym, val)
                bindingToList v = throwError (v, "Invalid binding") in
              fmap M.fromList $ mapM bindingToList bindings
     put (bds : obarray)
     val <- evalMultiExpr body
     obarray <- get
     put (tail obarray)
     return val

evalList (ExprSymbol "if") [condition, caseThen, caseElse] =
  do result <- evalExpr condition
     if result == ExprBool False
       then evalExpr caseElse
       else evalExpr caseThen

evalList (ExprSymbol "setq") [ExprSymbol sym, val] =
  do obarray <- get
     val <- evalExpr val
     put (oset sym val obarray)
     return val

evalList (ExprSymbol "intern") [ExprString name] =
  return $ ExprSymbol name

evalList (ExprSymbol "progn") args = evalMultiExpr args

evalList (ExprSymbol "quote") args =
  case length args of
    0 -> genericError
    1 -> return $ head args
    _ -> return $ ExprList args

evalList (ExprSymbol "funcall") (fn : args) =
  do fn' <- evalExpr fn
     evalList fn' args

evalList (ExprSymbol sym) args =
  case M.lookup sym bopPrimitives of
    Nothing ->
      if elem sym primitiveFunctions
      then do args' <- mapM evalExpr args
              evalPrimFunction sym args'
      else do fn <- evalExpr (ExprSymbol sym)
              evalList fn args
    Just bop -> foldM fn (head args) (tail args)
      where fn a b = do a' <- evalExpr a
                        b' <- evalExpr b
                        bop a' b'

-- User defined functions.
evalList (ExprList ((ExprSymbol "lambda")
                     : (ExprList params : body))) args =
  if (length params) /= (length args)
  then throwCurrentForm "Wrong number of arguments"
  else
    do obarray <- get
       bds <- let bindingToList (ExprSymbol sym, val) =
                    do val <- evalExpr val
                       return (sym, val)
                  bindingToList _ =
                    throwError (ExprList params, "Invalid parameter") in
                fmap M.fromList $ mapM bindingToList $ zip params args
       put (bds : obarray)
       val <- evalMultiExpr body
       obarray <- get
       put (tail obarray)
       return val

evalList fn _ = throwCurrentForm "Invalid function"

evalPrimFunction :: Symbol -> [Expression] -> ExpressionS

evalPrimFunction "not" [arg] = primNot arg

evalPrimFunction "not" _ = throwCurrentForm "Wrong type argument"

evalPrimFunction "car" [ExprList lst] =
  case length lst of
    0 -> return $ ExprList []
    _ -> return $ head lst
evalPrimFunction "car" [ExprString str] =
  case length str of
    0 -> return $ ExprString ""
    _ -> return $ ExprString [head str]
evalPrimFunction "car" _ = throwCurrentForm "Wrong type argument"

evalPrimFunction "cdr" [ExprList lst] =
  case length lst of
           0 -> return $ ExprList []
           _ -> return $ ExprList (tail lst)
evalPrimFunction "cdr" [ExprString str] =
  case length str of
           0 -> return $ ExprString ""
           _ -> return $ ExprString (tail str)
evalPrimFunction "cdr" _ = throwCurrentForm "Wrong type argument"

evalPrimFunction "symbolp" [arg] =
  case arg of
    ExprSymbol _ -> return $ ExprBool True
    _ -> return $ ExprBool False
    
evalPrimFunction "listp" [arg] =
  case arg of
    ExprList _ -> return $ ExprBool True
    _ -> return $ ExprBool False

evalPrimFunction "list" args = return $ ExprList args
evalPrimFunction "list" _ = throwCurrentForm "Wrong type argument"

evalPrimFunction "append" [ExprList l1, ExprList l2] =
  return $ ExprList (l1 ++ l2)
evalPrimFunction "append" [ExprString s1, ExprString s2] =
  return $ ExprString (s1 ++ s2)  
evalPrimFunction "append" _ = throwCurrentForm "Wrong type argument"

evalPrimFunction "take" [ExprInt n, ExprString str] =
  return $ ExprString (take n str)
evalPrimFunction "take" [ExprInt n, ExprList lst] =
  return $ ExprList (take n lst)
evalPrimFunction "take" _ = throwCurrentForm "Wrong type argument"

evalPrimFunction "drop" [ExprInt n, ExprString str] =
  return $ ExprString (drop n str)
evalPrimFunction "drop" [ExprInt n, ExprList lst] =
  return $ ExprList (drop n lst)
evalPrimFunction "drop" _ = throwCurrentForm "Wrong type argument"

evalPrimFunction "length" [ExprList lst] =
  return $ ExprInt (length lst)
evalPrimFunction "length" [ExprString str] =
  return $ ExprInt (length str)
evalPrimFunction "length" _ = throwCurrentForm "Wrong type argument"

evalPrimFunction _ _ = undefined

--- Eval

eval :: String -> String
eval program =
  let expr = parseString multiExprP program in
    case expr of
      Left err -> show err
      Right exp -> 
        case runExcept (evalStateT (evalMultiExpr exp) initialObarray) of
          Left (exp, err) -> err ++ ": " ++ prin1 exp
          Right exp -> prin1 exp

evalS :: Obarray -> String ->
  Either (Expression, String) (Expression, Obarray)
evalS obarray program =
  let expr = parseString multiExprP program in
    case expr of
      Left err -> throwError (nil, show err)
      Right exp -> 
        runExcept (runStateT (evalMultiExpr exp) obarray)

evalFile :: String -> IO String
evalFile file =
  do contents <- readFile file
     return $ eval contents

evalFileS :: String
  -> IO (Either (Expression, String) (Expression, Obarray))
evalFileS file =
  do contents <- readFile file
     return $ evalS initialObarray contents

evalInit :: String -> String -> IO String
evalInit file program =
  do contents <- readFile file
     return $ eval (contents ++ " " ++ program)
     
