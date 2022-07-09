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
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Cont

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

newtype Interpreter a = Interpreter
  {
    runInterpreter ::
      ExceptT Exception
      (ContT
       (Either Exception ())
       (StateT InterpreterState IO))
      a
  } deriving
  (
    Functor,
    Applicative,
    Monad,
    MonadIO,
    MonadBase IO,
    MonadState InterpreterState,
    MonadError Exception,
    MonadCont
  )

type Env = Map.Map Identifier (IORef Expr)


eval :: Expr -> Interpreter Value
eval = undefined


data InterpreterState = InterpreterState
  {
    isEnv :: Env
  }

initInterpreterState :: IO InterpreterState
initInterpreterState = InterpreterState <$> builtinEnv
