module Inlining ( inlineReturnFuncs
                , passes
) where

import           Language.ECMAScript3
import           Data.Generics.Uniplate.Data
import qualified Data.Map                                         as Map
import qualified          Data.Set as Set
import           Control.Monad.State
import           Debug.Trace

import Util
import Types
import Pass

passes = pass "inlining.return" (passify inlineReturnFuncs)
    [ passTest "constant function with literal"
        "function foo() { return 1; } foo();"
        "function foo() { return 1; } 1;"

    , passTest "Single argument - identity"
        "function foo(a) { return a; } foo(1);"
        "function foo(a) { return a; } 1;"

    , passTest "Single argument - expression"
        "function foo(a) { return a + 1; } foo(1);"
        "function foo(a) { return a + 1; } 1 + 1;"

    , passTest "Single argument - free variable"
        "function foo(a) { return b + 1; } foo(1);"
        "function foo(a) { return b + 1; } foo(1);"

    , passTest "Multiple arguments - not handled ATM"
        "function foo(a,b) { return a + b; } foo(1,2);"
        "function foo(a,b) { return a + b; } foo(1,2);"

    , passTest "Multiple arguments - single statement"
        "function f(a1,a2,a3) { return a1[a2](a3); } var a = [String]; f(a, 0, \"foo\");"
        "function f(a1,a2,a3) { return a1[a2](a3); } var a = [String]; a[0](\"foo\");"
    ]


-- TODO these should come from configuration
-- builtin objects that are not considered as free variables
builtinObjects = Set.fromList ["ActiveXObject"]

hasFreeVars vars expr =
  let argnames = Set.fromList [n | Id () n <- vars]
      refnames = Set.fromList [n | VarRef () (Id () n) <- universe expr]
  in not $ null (refnames `Set.difference` argnames `Set.difference` builtinObjects)


-- now handling only global functions
-- TODO refactor
inlineReturnFuncs :: JavaScript () -> JavaScript ()
inlineReturnFuncs tree = evalState (transformBiM f tree) globalConstFuncs
  where
    globalConstFuncs = Map.fromList $
      [(fname, ((map unId args), value)) | FuncStmtP fname args (SingleReturnP value) <- unJavaScript tree,
        (not $ hasFreeVars args value) && (length args <= 1)
      ] ++
      [(fname, ((map unId args), value)) | VarDeclStmtP fname (FuncExprP args (SingleReturnP value)) <- unJavaScript tree,
        (not $ hasFreeVars args value) && (length args <= 1)
      ]
  
    f :: Expression () -> State (Map.Map String ([String], (Expression ()))) (Expression ())
    f orig@(CallExprP fname []) = do
      fvalue <- gets (Map.lookup fname)
      return $ maybe orig snd fvalue

    f orig@(CallExprP fname args) = do
      fvalue <- gets (Map.lookup fname)
      case fvalue of
        Nothing -> return orig
        Just (fargs, body) -> let
            replaceVar :: Expression () -> Expression ()
            replaceVar o@(VarRef () (Id () varname)) = if varname `elem` fargs then head args else o
            replaceVar o = o
          in return $ transformBi replaceVar body
    f o = return o
