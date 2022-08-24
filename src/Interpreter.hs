{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
-- |

module Interpreter where

-- from transformers
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Cont
import Control.Monad.Base

import Control.Monad.IO.Class
-- import Control.Monad
-- import Control.Exception

-- from mtl
import Control.Monad.State (MonadState, StateT, evalStateT)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Except
import Control.Monad.Cont

import qualified Data.Sequence as Seq
import Data.Foldable (traverse_)

import AST
import qualified Data.Map as Map
import Data.IORef
import Data.Maybe (fromMaybe)

-- represents runtime values that vox variables can hold
data Value
  = Null
  | VBool Bool
  | VStr String
  | Num Integer
  | Function Identifier [Identifier] [Stmt] Env
  | BuiltinFunction Identifier Int ([Expr] -> Interpreter Value)

instance Show Value where
  show v = case v of
    Null -> "Null"
    VBool b -> show b
    VStr s -> s
    Num n -> show n
    Function name  _ _ _ -> "function " <> name
    BuiltinFunction name _ _ -> "function " <> name

type Identifier = String

instance Eq Value where
  Null == Null = True
  VBool b == VBool b' = b == b'
  VStr s == VStr s' = s == s'
  Num x == Num x' = x == x'
  _ == _ = False

data RuntimeExcept
  = Return Value
  | RuntimeError String

newtype Interpreter a = Interpreter
  {
    runInterpreter ::
      ExceptT RuntimeExcept
      (ContT
       (Either RuntimeExcept ())
       (StateT InterpreterState IO))
      a
  }
  deriving
  (
    Functor,
    Applicative,
    Monad,
    MonadIO,
    MonadBase IO,
    MonadState InterpreterState,
    MonadError RuntimeExcept,
    MonadCont
  )

type Env = Map.Map Identifier (IORef Value)

newtype InterpreterState = InterpreterState
  {
    isEnv :: Env
  }

initInterpreterState :: IO InterpreterState
initInterpreterState = InterpreterState <$> builtinEnv

builtinEnv :: IO Env
builtinEnv = do
  printFn <- newIORef $ BuiltinFunction "print" 1 executePrint
  return $ Map.fromList [("print", printFn)]

defineVar :: Identifier -> Value -> Interpreter ()
defineVar name val = do
  env <- State.gets isEnv
  env' <- defineVarEnv name val env
  setEnv env'

defineVarEnv :: Identifier -> Value -> Env -> Interpreter Env
defineVarEnv name val env = do
  valueRef <- liftIO (newIORef val)
  return $ Map.insert name valueRef env

lookupVar :: Identifier -> Interpreter Value
lookupVar name = State.gets isEnv >>= findValueRef name >>= liftIO . readIORef

assignVar :: Identifier -> Value -> Interpreter ()
assignVar name value =
  State.gets isEnv >>= findValueRef name >>= liftIO . flip writeIORef value

findValueRef :: Identifier -> Env -> Interpreter (IORef Value)
findValueRef name env =
  case Map.lookup name env of
    Just ref -> return ref
    Nothing -> throw $ "Unknown variable: " <> name

throw :: String -> Interpreter a
throw = throwError . RuntimeError

setEnv :: Env -> Interpreter ()
setEnv env = State.modify' $ \is -> is {isEnv = env}

executePrint :: [Expr] -> Interpreter Value
executePrint argsEs =
  eval (head argsEs) >>= liftIO . print >> return Null

-- main eval function
eval :: Expr -> Interpreter Value
eval e = case e of
  Nil -> pure Null
  CBool b -> pure $ VBool b
  CVar s -> pure $ VStr s
  CInt i -> pure $ Num i
  Variable v -> lookupVar v
  bin@BinOp {} -> evalBinOp bin
  uno@UnOp {} -> evalUnOp uno
  func@FunCall {} -> evalFunCall func

evalUnOp :: Expr -> Interpreter Value
evalUnOp ~(UnOp o e) = do
  operand <- eval e
  let errMsg m = m <> ": " <> show operand
  case (o, operand) of
    (Not, VBool b) -> pure $ VBool (not b)
    (Not, _) -> throw $ errMsg "can't negate value"

evalBinOp :: Expr -> Interpreter Value
evalBinOp ~(BinOp o e1 e2) = do
  lhs <- eval e1
  rhs <- eval e2
  let errMsg m = m <> ": " <> show lhs <> " and " <> show rhs
  case (o, lhs, rhs) of
    -- Addition
    (Add, Num n1, Num n2) -> pure $ Num (n1 + n2)
    (Add, VStr s1, VStr s2) -> pure $ VStr (s1 <> s2)
    (Add, VStr s1, _) -> pure $ VStr (s1 <> show rhs)
    (Add, _, VStr s2) -> pure $ VStr (show lhs <> s2)
    (Add, _, _) -> throw $ errMsg "Cannot add or append"
    -- Subtraction
    (Sub, Num n1, Num n2) -> pure $ Num (n1 - n2)
    (Sub, _, _) -> throw $ errMsg "Cannot subtract non-numeric values"
    -- Multiplication
    (Mul, Num n1, Num n2) -> pure $ Num (n1 * n2)
    (Mul, _, _) -> throw $ errMsg "Cannot multiply non-numeric values"
    -- Division
    (Div, Num n1, Num n2) -> pure $ Num (n1 `div` n2)
    (Div, _, _) -> throw $ errMsg "Cannot divide non-numeric values"
    -- Numeric Ordering Predicates
    (Less, Num n1, Num n2) -> pure $ VBool (n1 < n2)
    (Less, _, _) -> throw $ errMsg "Cannot compare non-numeric values"
    (LessEq, Num n1, Num n2) -> pure $ VBool (n1 <= n2)
    (LessEq, _, _) -> throw $ errMsg "Cannot compare non-numeric values"
    (Great, Num n1, Num n2) -> pure $ VBool (n1 > n2)
    (Great, _, _) -> throw $ errMsg "Cannot compare non-numeric values"
    (GreatEq, Num n1, Num n2) -> pure $ VBool (n1 >= n2)
    (GreatEq, _, _) -> throw $ errMsg "Cannot compare non-numeric values"
    -- Propositional Logic Operators
    (And, VBool b1, VBool b2) -> pure $ VBool (b1 && b2)
    (And, _, _) -> throw $ errMsg "Cannot perform boolean operation on these values"
    (Or, VBool b1, VBool b2) -> pure $ VBool (b1 || b2)
    (Or, _, _) -> throw $ errMsg "Cannot perform boolean operation on these values"
    -- Equality
    (Equal, _, _) -> pure $ VBool (lhs == rhs)
    (Neq, _, _) -> pure $ VBool (lhs /= rhs)


evalFunCall :: Expr -> Interpreter Value
evalFunCall = undefined

execute :: Stmt -> Interpreter ()
execute = \case
  Var name expr -> eval expr >>= defineVar name
  If e s1 s2 -> do
    cond <- eval e
    when (isTruthy cond) $
      traverse_ execute s1
  while@(While expr body) -> do
    cond <- eval expr
    when (isTruthy cond) $ do
      traverse_ execute body
      execute while
  ReturnStmt maybeExpr -> do
    maybeRet <- traverse eval maybeExpr
    throwError . Return . fromMaybe Null $ maybeRet
  FunStmt name params body -> do
    env <- State.gets isEnv
    defineVar name $ Function name params body env
  where
    isTruthy t = case t of
      Null -> False
      VBool b -> b
      _ -> True
