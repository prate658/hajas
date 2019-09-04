{-# LANGUAGE FlexibleContexts #-}
module Constants
  ( passes
  ) where

import Control.Applicative ((<|>))
import Control.Monad.State
import Data.Maybe
import Data.Generics.Uniplate.Data
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace
import Language.ECMAScript3

import Data.Semigroup ((<>))
import Pass
import Types
import Util

passes =
  pass
    "constants.fold"
    (passify foldConstants)
    [ passTest "Integer arithmetic" "2 + 2 * 3 - 1" "7;"
    , passTest "Integer arithmetic - division" "3 / 2" "1.5;"
    , passTest "String concatenation" "\"foo\" + \"bar\"" "\"foobar\";"
    -- this cannot be tested now in a simple way, since it requires two passes
    -- , passTest "Negation" "-3 + 1" "-2;"
    , passTest "With variable reference"
        "var a = b + \"a\" + \"b\""
        "var a = b + \"ab\";"
    ] <>
  pass
    "constants.propagate"
    (passify propagateConstants)
    [ passTest "simple" "var a = 1; a + 2;" "var a = 1; 1 + 2;"
    , passTest "global" "var a = 1; b = a;" "var a = 1; b = 1;"
    , passTest "Literal array" "var a = [1,2]; a[0];" "var a = [1,2]; 1;"
    , passTest
        "function"
        "var b = 3; function foo() { var a = 1; a + 2 + b; }"
        "var b = 3; function foo() { var a = 1; 1 + 2 + 3; }"
    , passTest
        "reassignment"
        "var a = 1; a + 2; a = 3; a + 3;"
        "var a = 1; 1 + 2; a = 3; 3 + 3;"
    , passTest
        "reassignment - conditional"
        "var a = 1; var b = a; if (b) { a = 2; } a + 2;"
        "var a = 1; var b = 1; if (1) { a = 2; } a + 2;"
    , passTest
        "reassignment - for"
        "var a = 1; var b = 2; for (i = 0; i < 6; i++) a = a + b + i;"
        "var a = 1; var b = 2; for (i = 0; i < 6; i++) a = a + 2 + i;"
    , passTest
        "reassignment - while"
        "var a = 1; var b = 2; while (true) a = a + b;"
        "var a = 1; var b = 2; while (true) a = a + 2;"
    , passTest
        "reassignment - with self"
        "var a = 1; a = a + 1; a = a + 2;"
        "var a = 1; a = 1 + 1; a = a + 2;"
    , passTest
        "When used as lvalue"
        "var a = 1; a += 1; foo(a);"
        "var a = 1; a += 1; foo(a);"
    , passTest
        "Global assignments in global scope"
        "var a = 1; var b = a;"
        "var a = 1; var b = 1;"
    , passTest
        "Inner global assignments propagate within scope"
        "function foo() { a = 1; b = a; } c = a;"
        "function foo() { a = 1; b = 1; } c = a;"
    ]

foldConstants :: JavaScript () -> JavaScript ()
foldConstants = transformBi f
  where
    f :: Expression () -> Expression ()
    -- "foo" + "bar" -> "foobar"
    f (InfixExpr () OpAdd (StringLit _ a) (StringLit _ b))
        = StringLit () (a ++ b)
    -- a + "a" + "b" -> a + "ab"
    -- lets just handle this as a special case for now...
    f (InfixExpr () OpAdd (InfixExpr () OpAdd ref@(VarRef () _) (StringLit () s1)) (StringLit () s2))
        = InfixExpr () OpAdd ref (StringLit () (s1 ++ s2))
    -- "2 + 2 * 3 / 3 -> 4"
    f node@(InfixExpr _ op (IntLit _ a) (IntLit _ b))
        = case op of
            OpAdd -> IntLit () (a + b)
            OpMul -> IntLit () (a * b)
            OpSub -> IntLit () (a - b)
            OpDiv -> NumLit () (fromIntegral a / fromIntegral b)
            _ -> node
    -- catch the rest
    f x = x

type ConstEnv = Map.Map String (Expression ())

localState m = do
    saved@(VarEnv sglobals slocals) <- get
    -- locals will become globals in the new scope
    put $ VarEnv (Map.union sglobals slocals ) Map.empty
    x <- m
    modified@(VarEnv mglobals mlocals) <- get
    -- take away old globals from mglobals
    -- take away modified globals from old globals
    put $ VarEnv (Map.withoutKeys sglobals (Map.keysSet mlocals)) (Map.withoutKeys mglobals (Map.keysSet sglobals))
    return x

data VarEnv = VarEnv
    { globals :: ConstEnv
    , locals :: ConstEnv
    } deriving (Eq, Show)

removeGlobal :: String -> State VarEnv ()
removeGlobal name = do
  (VarEnv g l) <- get
  put $ VarEnv (Map.withoutKeys g (Set.singleton name)) l
  return ()

removeLocal :: String -> State VarEnv ()
removeLocal name = do
  (VarEnv g l) <- get
  put $ VarEnv g (Map.withoutKeys l (Set.singleton name))
  return ()

putLocal :: String -> Expression () -> State VarEnv ()
putLocal name value = do
  (VarEnv g l) <- get
  put $ VarEnv g (Map.insert name value l)
  return ()

-- TODO rewrite this shit with a proper data flow analysis
propagateConstants tree = evalState (descendBiM f tree) (VarEnv Map.empty Map.empty)
  where
    f :: Statement () -> State VarEnv (Statement ())

    -- nodes that introduce a new var scope
    f node@FunctionStmt {} = localState (descendM f node)
    f node@(VarDeclStmtP _ (FuncExprP _ _)) = localState (descendM f node)
    f node@(IfSingleStmt _ cond block) = localState $ do
      newcond <- propagateVars cond
      newblock <- descendM f block
      return $ IfSingleStmt () newcond newblock

    f node@(VarDeclStmt _ stmts) = do
    --   -- first, propagate known constants to the var declarations
      newstmts <- propagateVars stmts
    --   -- take constants out of var declarations and put them to the state
      let consts = filter isConstDecl newstmts
      let vars = [(varid, expr) | VarDecl _ (Id _ varid) (Just expr) <- consts]
      forM_ vars (uncurry putLocal)
      -- traceState "after var decl"

      -- descend needed, because there might be anonymous functions in
      -- vardecls
      descendM f (VarDeclStmt () newstmts)
      -- return $ VarDeclStmt () newstmts

    f node@(ForStmt _ init test inc body) = localState $ do
        removeAssignedVars body
        descendM f node

    f node@(WhileStmt _ test body) = localState $ do
        removeAssignedVars body
        descendM f node

    -- assignment to a variable
    f node@(ExprStmt _ (AssignExpr _ OpAssign (LVar _ varname) value))
     = do
      -- this will propagate only to the value
      newnode <- propagateVars node
      removeGlobal varname
      if isLit value || isArrayLit value 
        then putLocal varname value
        else removeLocal varname
      -- traceState ("after expr: " ++ show value)
      return newnode
  
    -- any other operation cannot be handled yet
    f node@(ExprStmt _ (AssignExpr _ op (LVar _ varname) value)) = do
      newnode <- propagateVars node
      -- FIXME check?
      removeLocal varname
      removeGlobal varname
      return newnode
  
    -- else, descend first, then replace var refs with constants
    -- f node = trace ("TRACE: " ++ show node) $ do
    f node = do
      -- traceState ("node: " ++ show node)
      newnode <- descendM f node
      propagateVars newnode

removeAssignedVars tree = do
    let assigns = [name
                      | AssignExpr _ op (LVar _ name) val
                      <- universeBi tree :: [Expression ()]
                  ]
    forM_ assigns removeGlobal
    

-- TODO refactor
propagateVars tree = do
  s <- get
  -- transformBi goes bottom up, so replaceVar would always match first
  return $ transformBi (replaceVar s) . transformBi (replaceArrayVar s) $ tree


-- TODO refactor
replaceArrayVar :: VarEnv -> Expression () -> Expression ()
replaceArrayVar (VarEnv g l) orig@(BracketRef _ (VarRef _ (Id _ name)) (IntLit _ index))
  = case Map.lookup name l <|> Map.lookup name g of
    Just ( ArrayLit () items ) -> items !! index
    _ -> orig
replaceArrayVar _ orig = orig

replaceVar (VarEnv g l) orig@(VarRef _ (Id _ name))
  = fromMaybe orig $ mfilter (not.isArrayLit) (Map.lookup name l <|> Map.lookup name g)
replaceVar _ orig = orig

traceState :: (Show a, Show s) => a -> State s a
traceState x =
  state (\s -> trace ("trace: " ++ show x ++ " (" ++ show s ++ ")") (x, s))
