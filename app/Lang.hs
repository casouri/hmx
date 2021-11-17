module Lang where

import Text.Parsec hiding (State, between)
import Text.Parsec.String
import qualified Data.Map as M
import Control.Monad.Trans.State
import Control.Monad
import Debug.Trace (trace)
import Control.Monad.Except
import System.IO

data Expression 
   = ExprSymbol Symbol
   | ExprInt Int
   | ExprString String
   | ExprBool Bool
   | ExprList [Expression]
   deriving (Eq, Show)

type Symbol = String

type Obarray = [M.Map Symbol Expression]

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
nil = ExprBool False

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
    Nothing -> oset sym val rest
    Just _ -> M.adjust (const val) sym this : rest

initialObarray :: Obarray
initialObarray = [M.empty]

throwCurrentForm :: String -> ExpressionS
throwCurrentForm msg = do obarray <- get
                          form <- olookup "__current-form" obarray
                          throwError (form, msg)

genericError :: ExpressionS
genericError = throwCurrentForm "Malformed expression"

primAdd (ExprInt a) (ExprInt b) = return $ ExprInt (a + b)
primAdd _ _ = genericError
primSub (ExprInt a) (ExprInt b) = return $ ExprInt (a - b)
primSub _ _ = genericError
primMul (ExprInt a) (ExprInt b) = return $ ExprInt (a * b)
primMul _ _ = genericError
primDiv (ExprInt a) (ExprInt 0) = throwCurrentForm "Division by zero"
primDiv (ExprInt a) (ExprInt b) = return $ ExprInt (div a b)
primDiv _ _ = genericError
primEq (ExprInt a) (ExprInt b) = return $ ExprBool (a == b)
primEq _ _ = genericError
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
                            ("or", primOr)]

primitives :: [Symbol]
primitives = ["if", "let", "set", "not", "progn"] ++ M.keys bopPrimitives

--- Parse

intP :: Parser Expression
intP = fmap (ExprInt . read) (spaces >> many1 digit)

symbolP :: Parser Expression
symbolP = fmap ExprSymbol (spaces >> many1 (noneOf "\"() "))

stringP :: Parser Expression
stringP = do spaces
             char '"'
             str <- many1 (noneOf "\"")
             char '"'
             return $ ExprString str
  
boolP :: Parser Expression
boolP = try (spaces >> string "true" >> return (ExprBool True))
        <|> (spaces >> string "false" >> return (ExprBool False))

exprP :: Parser Expression
exprP   = try intP
          <|> try boolP
          <|> try symbolP
          <|> try stringP
          <|> do spaces
                 char '('
                 stuff <- try multiExprP <|> return []
                 spaces
                 char ')'
                 return $ ExprList stuff

multiExprP :: Parser [Expression]
multiExprP = many1 exprP

parseString :: Parser a -> String -> Either ParseError a 
parseString p s = runParser p () "REPL" s 

parseFile :: FilePath -> IO (Either ParseError [Expression])
parseFile f = parseFromFile multiExprP f

--- Eval

evalExpr :: Expression -> ExpressionS
                                 
evalExpr v@(ExprList []) = return v
evalExpr v@(ExprList ((ExprSymbol "lambda") : _)) = return v
evalExpr v@(ExprList (fn : args)) =
  do fn <- evalExpr fn
     obarray <- get
     put $ oset "__current-form" v obarray
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
evalMultiExpr [] = return $ ExprInt 0
evalMultiExpr [expr] = evalExpr expr
evalMultiExpr (x:xs) = evalExpr x >> evalMultiExpr xs

evalList :: Expression -> [Expression] -> ExpressionS

bindingToList :: Expression -> Maybe [(Symbol, Expression)]
bindingToList (ExprList lst) = mapM toTuple lst where
  toTuple (ExprList [ExprSymbol sym, val]) = Just (sym, val)
  toTuple exp = Nothing
bindingToList _ = Nothing

evalList (ExprSymbol "let") (bindings : body) =
  do obarray <- get
     case bindingToList bindings of
       Nothing -> throwError (bindings, "Malformed bindings form")
       Just bds -> put $ M.fromList bds : obarray
     val <- evalMultiExpr body
     obarray <- get
     put $ tail obarray
     return val
evalList (ExprSymbol "let") _ = genericError

evalList (ExprSymbol "if") [condition, caseThen, caseElse] =
  do result <- evalExpr condition
     if result == ExprBool True
       then evalExpr caseThen
       else evalExpr caseElse
evalList (ExprSymbol "if") _ = genericError       

evalList (ExprSymbol "set") [ExprSymbol sym, val] =
  do obarray <- get
     put $ oset sym val obarray
     return  $ ExprInt 0
evalList (ExprSymbol "set") _ = genericError

evalList (ExprSymbol "progn") args = evalMultiExpr args

-- Primitive functions.
evalList (ExprSymbol "not") [arg] =
  do arg' <- evalExpr arg
     primNot arg'
evalList (ExprSymbol "not") _ = genericError

evalList (ExprSymbol sym) args =
  case M.lookup sym bopPrimitives of
    Nothing -> undefined -- Shouldn’t happen.
    Just bop -> foldM fn (head args) (tail args)
      where fn a b = do a' <- evalExpr a
                        b' <- evalExpr b
                        bop a' b'
    

-- User defined functions.
evalList (ExprList ((ExprSymbol "lambda")
                     : (ExprList params : body))) args =
  if (length params) /= (length args)
  then throwCurrentForm "Wrong number of arguments"
  else evalExpr letForm
  where fn (sym, val) = ExprList [sym, val]
        bindings = ExprList (map fn (zip params args))
        letForm = ExprList $ ExprSymbol "let" : (bindings : body)

evalList _ _ = undefined
          

eval1 :: String -> String
eval1 program =
  let expr = parseString exprP program in
    case expr of
      Left err -> show err
      Right exp ->
        case runExcept (evalStateT (evalExpr exp) initialObarray) of
          Left (exp, err) -> err ++ ": " ++ prin1 exp
          Right exp -> prin1 exp

eval :: String -> String
eval program =
  let exprList = parseString multiExprP program in
    case exprList of
      Left err -> show err
      Right exp ->
        case runExcept (evalStateT (evalMultiExpr exp) initialObarray) of
          Left (exp, err) -> err ++ ": " ++ prin1 exp
          Right exp -> prin1 exp

evalFile :: String -> IO String
evalFile file =
  do handle <- openFile file ReadMode
     contents <- hGetContents handle
     return $ eval contents

