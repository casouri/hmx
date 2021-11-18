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

type Buffer = (String, Int)

type World = (Buffer, Obarray)

type Error = (Expression, String)

type StateExcept e s a = StateT s (Except e) a
type ExpressionS = StateExcept (Expression, String) World Expression

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

initialWorld :: World
initialWorld = (("", 0), [M.empty])

--- Error

throwCurrentForm :: String -> ExpressionS
throwCurrentForm msg = do (buf, obarray) <- get
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
primitives = ["if", "let", "setq", "not", "progn",
              "car", "cdr", "list", "quote", "insert", "delete",
              "goto", "point", "buffer-string"]
             ++ M.keys bopPrimitives


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
             str <- many1 (noneOf "\"")
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
     (buf, obarray) <- get
     put (buf, oset "__current-form" v obarray)
     evalList fn args
evalExpr v@(ExprInt _) = return v
evalExpr v@(ExprBool _) = return v
evalExpr v@(ExprString _) = return v
evalExpr v@(ExprSymbol sym) =
  if elem sym primitives
  then return v
  else do (buf, obarray) <- get
          olookup sym obarray

evalMultiExpr :: [Expression] -> ExpressionS
evalMultiExpr exprs = fmap last (mapM evalExpr exprs)

evalList :: Expression -> [Expression] -> ExpressionS

evalList (ExprSymbol "let") ((ExprList bindings) : body) =
  do (buf, obarray) <- get
     bds <- let bindingToList (ExprList [ExprSymbol sym, val]) =
                  do val <- evalExpr val
                     return (sym, val)
                bindingToList v = throwError (v, "Invalid binding") in
              fmap M.fromList $ mapM bindingToList bindings
     put (buf, (trace (show bds) bds) : obarray)
     val <- evalMultiExpr body
     (buf, obarray) <- get
     put (buf, tail obarray)
     return val

evalList (ExprSymbol "if") [condition, caseThen, caseElse] =
  do result <- evalExpr condition
     if result == ExprBool False
       then evalExpr caseElse
       else evalExpr caseThen

evalList (ExprSymbol "setq") [ExprSymbol sym, val] =
  do (buf, obarray) <- get
     val <- evalExpr val
     put (buf, oset sym val obarray)
     return nil

evalList (ExprSymbol "intern") [ExprString name] =
  return $ ExprSymbol name

evalList (ExprSymbol "progn") args = evalMultiExpr args

-- Primitive functions.
evalList (ExprSymbol "not") [arg] =
  do arg <- evalExpr arg
     primNot arg

evalList (ExprSymbol "car") [arg] =
  do val <- evalExpr arg
     case val of
       ExprList lst -> 
         case length lst of
           0 -> return $ ExprList []
           _ -> return $ head lst
       _ -> throwCurrentForm "Wrong type argument"

evalList (ExprSymbol "cdr") [arg] =
  do val <- evalExpr arg
     case val of
       ExprList lst -> 
         case length lst of
           0 -> return $ ExprList []
           _ -> return $ ExprList (tail lst)
       _ -> throwCurrentForm "Wrong type argument"

evalList (ExprSymbol "listp") [arg] =
  do val <- evalExpr arg
     case val of
       ExprList _ -> return $ ExprBool True
       _ -> return $ ExprBool False

evalList (ExprSymbol "list") args = fmap ExprList $ mapM evalExpr args

evalList (ExprSymbol "quote") args =
  case length args of
    0 -> genericError
    1 -> return $ head args
    _ -> return $ ExprList args

evalList (ExprSymbol "insert") [arg] =
  do arg' <- evalExpr arg
     ((body, pos), ob) <- get
     case arg' of
       ExprString txt ->
         do put ((insert body pos txt, pos + length txt), ob)
            return arg'
       _ -> throwCurrentForm "Wrong type argument"

evalList (ExprSymbol "goto") [arg] =
  do arg' <- evalExpr arg
     case arg' of
       ExprInt delta ->
         do ((body, pos), ob) <- get
            put ((body, pos + delta), ob)
            return arg'
       _ -> throwCurrentForm "Wrong type argument"

evalList (ExprSymbol "delete") [arg] =
  do arg' <- evalExpr arg
     case arg' of
       ExprInt len ->
         do ((body, pos), ob) <- get
            put ((delete body pos len, pos - len), ob)
            return arg'
       _ -> throwCurrentForm "Wrong type argument"

evalList (ExprSymbol "point") [] =
  do ((body, pos), ob) <- get
     return $ ExprInt pos

evalList (ExprSymbol "buffer-string") [] =
  do ((body, pos), ob) <- get
     return $ ExprString body

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
  else
    do (buf, obarray) <- get
       bds <- let bindingToList (ExprSymbol sym, val) =
                    do val <- evalExpr val
                       return (sym, val)
                  bindingToList _ =
                    throwError (ExprList params, "Invalid parameter") in
                fmap M.fromList $ mapM bindingToList $ zip params args
       put (buf, bds : obarray)
       val <- evalMultiExpr body
       (buf, obarray) <- get
       put (buf, tail obarray)
       return val

evalList _ _ = undefined

eval :: String -> String
eval program =
  let expr = parseString multiExprP program in
    case expr of
      Left err -> show err
      Right exp -> 
        case runExcept (evalStateT (evalMultiExpr exp) initialWorld) of
          Left (exp, err) -> err ++ ": " ++ prin1 exp
          Right exp -> prin1 exp

evalS :: World -> String ->
  Either (Expression, String) (Expression, World)
evalS world program =
  let expr = parseString multiExprP program in
    case expr of
      Left err -> throwError (nil, show err)
      Right exp -> 
        runExcept (runStateT (evalMultiExpr exp) world)

evalFile :: String -> IO String
evalFile file =
  do contents <- readFile file
     return $ eval contents
