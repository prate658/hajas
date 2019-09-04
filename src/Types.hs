{-# LANGUAGE PatternSynonyms #-}
module Types (
  pattern FuncStmtP,
  pattern FuncExprP,
  pattern SingleReturnP,
  pattern VarDeclStmtP,
  pattern CallExprP,
) where

import           Language.ECMAScript3


pattern FuncStmtP name args stmts = FunctionStmt () (Id () name) args stmts
pattern FuncExprP args stmts = FuncExpr () Nothing args stmts
pattern SingleReturnP expr = [ReturnStmt () (Just expr)]
pattern VarDeclStmtP name expr = VarDeclStmt () [VarDecl () (Id () name) (Just expr)]
pattern CallExprP name args = CallExpr () (VarRef () (Id () name)) args
