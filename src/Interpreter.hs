{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |

module Interpreter where

-- from transformers
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Cont
import Control.Monad.Base

import Control.Monad.IO.Class
import Control.Monad
-- import Control.Exception

-- from mtl
import Control.Monad.State (MonadState, StateT, evalStateT)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Error
import Control.Monad.Cont

import qualified Data.Sequence as Seq

import AST
import qualified Data.Map as Map
import Data.IORef

-- represents runtime values that vox variables can hold
data Value
  = Null
  | VBool Bool
  | VStr String
  | Num Integer
  | Function Identifier [Identifier] [Stmt] Env
  | BuiltinFunction Identifier Int ([Expr] -> Interpreter Value)

instance Show Value where
  show = \v -> case v of
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

data InterpreterState = InterpreterState
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
eval = \e -> case e of
  Nil -> pure Null
  CBool b -> pure $ VBool b
  CVar s -> pure $ VStr s
  CInt i -> pure $ Num i
  Variable v -> lookupVar v
