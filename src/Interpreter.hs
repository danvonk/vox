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
import Control.Exception
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

-- initInterpreterState :: IO InterpreterState
initInterpreterState = InterpreterState <$> preludeEnv <*> newIORef Seq.empty

preludeEnv :: Env
preludeEnv = Map.empty

defineVar :: Identifier -> Value -> Interpreter ()
defineVar name val = do
  env <- State.gets isEnv
  env' <- defineVarEnv name val env
  setEnv env'

defineVarEnv :: Identifier -> Value -> Env -> Interpreter Env
defineVarEnv name val env = do
  valueRef <- newIORef val
  return $ Map.insert name valueRef env

setEnv :: Env -> Interpreter ()
setEnv env = State.modify' $ \is -> is {isEnv = env}

-- main eval function
eval :: Expr -> Interpreter Value
eval = undefined
