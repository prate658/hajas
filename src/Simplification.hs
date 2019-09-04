module Simplification
  ( simplifyExpressions
  , evaluateComparisons
  , passes
  ) where

import           Language.ECMAScript3
import           Language.ECMAScript3.Syntax.Annotations
import           Data.Generics.Uniplate.Data
import           Data.List
import           Data.List.Split
import           Data.Char

import Data.Semigroup ((<>))
import Pass

passes = pass "simp.expressions" (passify simplifyExpressions) 
            [ passTest "Literal array indexed" "[1, 2][0];" "1;"
            , passTest "String literal bracketref to dotref" "obj[\"length\"];" "obj.length;"
            , passTest "Calculate string literal length" "\"foo\".length ;" "3;"
            , passTest "String literal split to array literal" "\"foo,bar\".split(\",\");" "[\"foo\",\"bar\"];"
            , passTest "String object constructor to literal" "String(\"foo\");" "\"foo\";"
            , passTest "String literal replace char" "\"foo,bar\".replace(\",\", \";\");" "\"foo;bar\";"
            , passTest "List expression" "(\"foo\", \"bar\")" "\"bar\";"
            , passTest "Immediate call of a function expression with only return" 
                "a = function () { return \"foo\";}();"
                "a = \"foo\";"
            , passTest "Calculate array literal length" "[0, 1, 2].length;" "3;"
            , passTest "Array constructor with elements" "new Array(1,2,3);" "[1,2,3];"
            , passTest "Array constructor with size" "new Array(1);" "new Array(1);"
            , passTest "Function constructor" "a = new Function(\"return 1;\");" "a = function () { return 1; };"
            , passTest "Function constructor with args" "a = new Function(\"a\", \"return 1;\");" "a = function (a) { return 1; };"
            , passTest "Function constructor with args" "a = new Function(\"a\", \"b\", \"return 1;\");" "a = function (a,b) { return 1; };"
              -- TODO this should still fail?
            , passTest "Function constructor with args" "a = new Function(\"a,b\", \"return 1;\");" "a = function (a,b) { return 1; };"
            , passTest "array negation" "![];" "false;"
            , passTest "boolean negation" "!false;" "true;"
            ]
         <> pass "simp.comparisons" (passify evaluateComparisons)
              [ passTest "true == true" "true == true;" "true;"
              , passTest "true == false" "true == false;" "false;"
              , passTest "null == true" "null == false;" "false;"
              , passTest "1 == 1" "1 == 1" "true;"
              , passTest "2 > 1" "2 > 1" "true;"
              , passTest "3 <= 3" "3 <= 3" "true;"
              -- , passTest "'1' == 1" evaluateComparisons "'1' == 1" "true;"
              -- , passTest "0 == false" evaluateComparisons "0 == false" "true;"
              -- , passTest "0 == null" evaluateComparisons "0 == null" "false;"
              -- , passTest "0 == undefined" evaluateComparisons "0 == undefined" "false;"
              , passTest "null == undefined" "null == undefined" "true;"
              , passTest "\"foo\" == \"foo\"" "\"foo\" == \"foo\"" "true;"
              ]
         <> pass "simp.eval" (passify evaluateEval)
              [ passTest "eval statements" "eval(\"a = 1;\")" "{ a = 1; }"
              ]


isString (StringLit () _) = True
isString _                = False

simplifyExpressions :: JavaScript () -> JavaScript ()
simplifyExpressions = transformBi f
  where
    f :: Expression () -> Expression ()
    -- ("foo", "bar", "baz") -> "baz"
    f (ListExpr () things) = last things

    -- ["a", "b"][0] -> "a"
    f (BracketRef _ (ArrayLit _ items) (IntLit _ num)) = items !! num

    -- obj["length"] -> obj.length
    f (BracketRef _ vref (StringLit _ it)) = DotRef () vref (Id () it)

    -- "foo,bar".split(",") -> ["foo", "bar"]
    f (CallExpr _ (DotRef _ (StringLit _ str) (Id _ "split")) [StringLit _ s]) = ArrayLit () (map (StringLit ()) (splitOn s str))

    -- "foo".length -> 3
    f (DotRef _ (StringLit _ str) (Id _ "length")) = IntLit () (length str)

    -- String.fromCharCode(0x40, 60) -> "????"
    f node@(CallExpr _ (DotRef _ (VarRef _ (Id _ "String")) (Id _ "fromCharCode")) things)
      | all isInt things = StringLit () (map toChar things)
      | otherwise = node
      where
        isInt (IntLit () _) = True
        isInt _             = False
        toChar (IntLit () i) = chr i

    -- "foo,bar".replace(",",";")
    f (CallExpr _ (DotRef _ (StringLit _ str) (Id _ "replace")) [StringLit _ a, StringLit _ b]) = StringLit () (intercalate b $ splitOn a str)

    -- String("foo") -> "foo"
    f (CallExpr _ (VarRef _ (Id _ "String")) [StringLit _ str]) = StringLit () str

    -- new Function(body) -> function () { body }
    -- XXX this is not accurate: "Functions created with the Function constructor do not create closures to their creation contexts; they always are created in the global scope."
    -- but we do it to get the variable references visible for dead code removal
    f orig@(NewExpr () (VarRef () (Id () "Function")) eargs)
      | all isString eargs = createFunc [str | StringLit () str <- eargs]
      | otherwise = orig
      where
        createFunc [body] = FuncExpr () Nothing [] (parseBody body)
        createFunc args = FuncExpr () Nothing (map (Id ()) (init args)) (parseBody . last $ args)

        parseBody body = case parseFromString (body) of
          Left e -> error $ show e
          Right b -> (unJavaScript . removeAnnotations) b


    -- function () { return "foo;"}() -> "foo"
    f (CallExpr () (FuncExpr () Nothing [] [ReturnStmt () (Just expr)]) []) = expr

    -- [0,1,2].length -> 3
    f (DotRef _ (ArrayLit _ array) (Id _ "length")) = IntLit () (length array)

    -- new Array(1,2,3) -> [1,2,3]
    f node@(NewExpr () (VarRef () (Id () "Array")) args)
      | length args > 1 = ArrayLit () args
      | otherwise = node

    f (PrefixExpr _ PrefixMinus (IntLit _ a)) = IntLit () (-a)
    -- ![] -> false
    f (PrefixExpr _ PrefixLNot (ArrayLit _ [])) = BoolLit () False
    -- !true -> false
    f (PrefixExpr _ PrefixLNot (BoolLit _ b)) = BoolLit () (not b)

    -- catch the rest
    f x = x

evaluateEval :: JavaScript () -> JavaScript ()
evaluateEval = transformBi evalExpr
  where
    evalExpr orig@(ExprStmt _ (CallExpr () (VarRef () (Id () "eval")) [StringLit () code]))
      = case parseCode code of
          Left e -> error $ show e
          Right b -> b
      where
        parseCode body = BlockStmt () . unJavaScript . removeAnnotations <$> parseFromString body
    evalExpr o = o

evaluateComparisons :: JavaScript () -> JavaScript ()
evaluateComparisons = transformBi f
  where
    f :: Expression () -> Expression ()

    f (InfixExpr () OpEq (StringLit () a) (StringLit () b)) = BoolLit () (a == b)
    f (InfixExpr () OpEq (BoolLit () a) (BoolLit () b)) = BoolLit () (a == b)
    f (InfixExpr () OpEq (NullLit ()) (VarRef () (Id () "undefined"))) = BoolLit () True
    f (InfixExpr () OpEq (NullLit ()) (BoolLit () _)) = BoolLit () False
    f (InfixExpr () OpEq (VarRef () (Id () "undefined")) (BoolLit () _) ) = BoolLit () False

    f node@(InfixExpr () op (IntLit () a) (IntLit () b)) = case op of
      OpEq -> BoolLit () (a == b)
      OpLT -> BoolLit () (a < b)
      OpGT -> BoolLit () (a > b)
      OpGEq -> BoolLit () (a >= b)
      OpLEq -> BoolLit () (a <= b)
      _ -> node
    -- catch the rest
    f x = x
