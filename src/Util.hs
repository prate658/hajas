module Util
  ( isLit
  , isArrayLit
  , isBreakStmt
  , isConstDecl
  ) where

import Control.Monad.State
import qualified Data.Set as Set
import Language.ECMAScript3

-- also these objects can be treated as literals when propagating
specialVarIds = Set.fromList ["undefined", "Function"]

isLit :: Expression () -> Bool
isLit StringLit {} = True
isLit BoolLit {} = True
isLit NullLit {} = True
isLit IntLit {} = True
isLit (VarRef _ (Id _ varid)) = Set.member varid specialVarIds
isLit _ = False

isArrayLit (ArrayLit _ items) = all isLit items
isArrayLit _ = False

isConstDecl :: VarDecl () -> Bool
isConstDecl (VarDecl _ (Id _ _) (Just expr)) = isLit expr || isArrayLit expr
isConstDecl _ = False

isBreakStmt (BreakStmt _ _) = True
isBreakStmt _ = False
