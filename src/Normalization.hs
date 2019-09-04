module Normalization
  ( globalAssignmentToDecl
  , passes
  ) where

import           Language.ECMAScript3
import qualified          Data.Set as Set
import           Control.Monad.State

-- import Data.Semigroup ((<>))
import Pass

passes =  pass "normalization.global_assignment" (passify globalAssignmentToDecl)
    [ passTest "Global assignment to declaration" 
        "a = 1; a = 2;"
        "var a = 1; a = 2;"
    ]

globalAssignmentToDecl :: JavaScript () -> JavaScript ()
globalAssignmentToDecl tree = Script () $ evalState (mapM f (unJavaScript tree)) Set.empty
  where
    f :: Statement () -> State (Set.Set String) (Statement ())
    f orig@(ExprStmt _ (AssignExpr _ OpAssign (LVar _ varname) value)) =  do
      ismember <- gets (Set.member varname)
      if ismember
        then return orig
        else do
          modify (Set.insert varname)
          return $ VarDeclStmt () [VarDecl () (Id () varname) (Just value)]

    -- TODO handle multiple decls
    f orig@(VarDeclStmt _ [VarDecl _ (Id _ varname) (Just _)]) = do
      modify (Set.insert varname)
      return orig

    f o =  return o
