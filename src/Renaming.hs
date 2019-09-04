module Renaming
  ( renameFuncLocals
  , renameFuncArgs
  , passes
  ) where 

import qualified Data.Map                                         as M
import           Language.ECMAScript3
import           Data.Generics.Uniplate.Data
import Data.Semigroup ((<>))
import Control.Monad.Reader

import Pass

type NameMap = M.Map String String

passes = pass "rename.func.locals" (passify renameFuncLocals) 
            [ passTest "Rename function locals" 
                "function foo() { var one; foo(one); one = global; }"
                "function foo() { var local_0; foo(local_0); local_0 = global; }"
            ]
         <> pass "rename.func.args" (passify renameFuncArgs)
            [ passTest "Rename function arguments" 
                "function foo(bar,baz) { foo(bar); }"
                "function foo(arg_0,arg_1) { foo(arg_0); }"
            ]
         <> pass "rename.replacements" (replaceFromConfig) []

renameFuncLocals :: JavaScript () -> JavaScript ()
renameFuncLocals tree = transformBi renameLocalVars tree
  where
    renameLocalVars :: Statement () -> Statement ()
    renameLocalVars func@(FunctionStmt _ _ _ stmts) = (transformBi (renameVarDecl varmap)) . (transformBi (renameVar varmap)) $ func
      where
        varmap = localsMap func

        localsMap :: Statement () -> NameMap
        localsMap t = M.fromList $ zip (map varDeclToId (varDeclarations t)) (varNameGenerator "local_")

        varDeclarations :: Statement () -> [VarDecl ()]
        -- why does this need to return a data instance (?) for type checker to be happy?
        varDeclarations t = [a | a@VarDecl{} <- universeBi t]
    renameLocalVars a = a


renameFuncArgs :: JavaScript () -> JavaScript ()
renameFuncArgs tree = transformBi renameArgs tree
  where
    renameArgs :: Statement () -> Statement ()
    renameArgs orig@(FunctionStmt a b args stmts) =
      let argsPairs = zip [n | Id () n <- args] (varNameGenerator "arg_")
          argsMap = M.fromList argsPairs
          (FunctionStmt _ _ _ newstmts) = transformBi (renameVar argsMap) orig
          newargs = [Id () i | i <- (map snd argsPairs)]
      in FunctionStmt a b newargs newstmts
    renameArgs a = a


renameVarDecl :: NameMap -> VarDecl () -> VarDecl ()
renameVarDecl m orig@(VarDecl _ (Id _ name) val) = VarDecl () (Id () (M.findWithDefault name name m)) val

renameVar :: NameMap -> Expression () -> Expression ()
renameVar m orig@(VarRef _ (Id _ name)) = VarRef () (Id () (M.findWithDefault name name m))
renameVar m orig@(AssignExpr () op (LVar () name) val) = AssignExpr () op (LVar () (M.findWithDefault name name m)) val
renameVar _ x = x

varDeclToId (VarDecl _ (Id _ i) _) = i

varNameGenerator :: String -> [String]
varNameGenerator prefix = map ((prefix ++) . show) [0..]


replaceFromConfig :: JavaScript () -> Pass (JavaScript ())
replaceFromConfig tree = transformBiM replaceId tree
    where
        replaceId :: Id () -> Pass (Id ())
        replaceId orig@(Id _ s) = do
            replacemap <- asks replacements
            return $ maybe orig (Id ()) (M.lookup s =<< replacemap)
