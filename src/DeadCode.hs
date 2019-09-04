module DeadCode
    ( passes
    ) where

import qualified          Data.Set as Set
import           Data.List
import           Control.Monad.Reader
import           Language.ECMAScript3
import           Data.Generics.Uniplate.Data
import Debug.Trace
import Data.Semigroup ((<>))

import Util
import Pass

passes = pass "deadcode.unused.globals" (passify removeUnusedGlobals)
            [ passTest "Unused global assignements" 
                "a = 1; b = 2; foo(a);"
                "a = 1; foo(a);"
            ]
         <> pass "deadcode.unused.vars" (passify removeUnusedVars)
              [ passTest "Unused vars" 
                    "var a = 1;" "var ;"
              , passTest "Unused vars - when only lvar" 
                  "var a = 1; a += 1;" "var a = 1; a += 1;"
              , passTest "Unused vars in function" 
                  "function foo() { var a = 1;}"
                  "function foo() { var ; }"
               ]
         <> pass "deadcode.const.if" (passify constIfStmts)
              [ passTest "Single if statement: true" 
                  "if (true) 1;" "{ 1; }"
              , passTest "Single if statement: false" 
                  "if (false) 1;" ";"
              , passTest "If else: true" 
                  "if (true) 1; else 2;" "1;"
              , passTest "If else: false" 
                  "if (false) 1; else 2;" "2;"
              ]
         <> pass "deadcode.const.switch" (passify constSwitch)
              [ passTest "Switch statement with literal" 
                  "switch(null){case null: 1; break; case false: 2; break;}"
                  "{ 1; }"
              ]
         <> pass "deadcode.flatten.blocks" (passify flattenBlocks)
              [ passTest "Single statement blocks" 
                  "{1;}" "1;"
              ]
         <> pass "deadcode.unused.funcs" (passify removeUnusedFunctions) []
         <> pass "deadcode.empty.statements" (passify removeEmptyStmts)
              [ passTest "Empty statements" ";" ""
              ]

isEmptyStmt (VarDeclStmt _ []) = True
isEmptyStmt (EmptyStmt _) = True
isEmptyStmt _ = False

removeEmptyStmts :: JavaScript() -> JavaScript ()
removeEmptyStmts = filterStmts (not.isEmptyStmt)

filterStmts :: (Statement () -> Bool) -> JavaScript() -> JavaScript ()
filterStmts ffunc tree = let
  (Script _ stmts) = transformBi e $ transformBi f $ tree

  f :: Statement () -> Statement ()
  f (BlockStmt _ stmts) = BlockStmt () (filter ffunc stmts)
  f (FunctionStmt _ name args stmts) = FunctionStmt () name args (filter ffunc stmts)
  f x = x

  -- function expression has to be handled separately...
  e :: Expression () -> Expression ()
  e (FuncExpr () name args stmts) = FuncExpr () name args (filter ffunc stmts)
  e x = x

  -- finally the top level statements
  in Script () (filter ffunc stmts)


-- remove block statement with only a single statement in it
flattenBlocks :: JavaScript() -> JavaScript ()
flattenBlocks tree = transformBi f tree
  where
    f :: Statement () -> Statement ()
    f node@(BlockStmt _ stmts)
      | length stmts == 1 = head stmts
      | otherwise = node
    f x = x

constIfStmts :: JavaScript() -> JavaScript ()
constIfStmts tree = transformBi f tree
  where
    f :: Statement () -> Statement ()
    f node@(IfSingleStmt _ (BoolLit _ bool) stmt) = case bool of
      True -> BlockStmt () [stmt]
      False -> EmptyStmt ()
    f node@(IfStmt _ (BoolLit _ bool) ifstmt elsestmt) = case bool of
      True -> ifstmt
      False -> elsestmt
    f x = x

constSwitch :: JavaScript() -> JavaScript ()
constSwitch tree = transformBi f tree
  where
    f :: Statement () -> Statement ()
    f node@(SwitchStmt () switch cases)
      -- if switch is literal, try to find the corresponding case clause
      | isLit switch = case find (\(CaseClause _ c _) -> c == switch) cases of
          -- if found, turn it into a block statement with break removed
          (Just (CaseClause _ _ stmts)) -> BlockStmt () (filter (not . isBreakStmt) stmts)
          Nothing -> node
      | otherwise = node
    f x = x

removeUnusedVars :: JavaScript() -> JavaScript ()
removeUnusedVars tree = runReader (transformBiM f tree) (Set.fromList referencedVars)
  where
    referencedVars = [name | (VarRef _ (Id _ name)) <- universeBi tree :: [Expression ()]]
      ++ [name | (LVar () name) <- universeBi tree]

    f :: Statement () -> Reader (Set.Set String) (Statement ())
    f node@(VarDeclStmt _ stmts) = do
      s <- ask
      return $ VarDeclStmt () (filter (\(VarDecl _ (Id _ name) _) -> (Set.member name s)) stmts)
    f x = return x

removeUnusedGlobals :: JavaScript() -> JavaScript ()
removeUnusedGlobals tree = Script () (filter stmtPred (unJavaScript tree))
  where
    referencedVars = Set.fromList [name | (VarRef _ (Id _ name)) <- universeBi tree :: [Expression ()]]
    stmtPred (ExprStmt () (AssignExpr _ OpAssign (LVar _ varname) _)) = Set.member varname referencedVars
    stmtPred _ = True


removeUnusedFunctions :: JavaScript() -> JavaScript ()
removeUnusedFunctions tree = let
    referencedVars = [name | (VarRef _ (Id _ name)) <- universeBi tree :: [Expression ()]]
    isUnreferencedFunc (FunctionStmt () (Id () funcname) _ _) = not (Set.member funcname (Set.fromList referencedVars))
    isUnreferencedFunc _ = False

    in filterStmts (not.isUnreferencedFunc) tree
