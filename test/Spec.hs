import Test.Tasty
import Test.Tasty.HUnit
import           Language.ECMAScript3 hiding (parse)
import           Language.ECMAScript3.Syntax.Annotations

import Lib
import qualified Simplification as S
import qualified Constants as C
import qualified DeadCode as D
import qualified Inlining as I
import qualified Normalization as N
import qualified Renaming as R
import qualified Pass as P

main :: IO ()
main = defaultMain tests

replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map $ \c -> if c == a then b else c

removeNewlines :: String -> String
removeNewlines = replace '\n' ' '

trimSpaces :: String -> String
trimSpaces [] = []
trimSpaces [x] = [x]
trimSpaces (a:b:xs)
  | a == ' ' && a == b = trimSpaces (b:xs)
  | otherwise = a:trimSpaces (b:xs)

trimOutput = trimSpaces . removeNewlines

testjs desc f input output = testCase desc $ do
    case parseFromString input of
          Left a -> assertFailure (show a)
          Right a -> assertEqual "Not equals" output $ (trimSpaces . removeNewlines . toString . f . removeAnnotations) a

testToCase f (P.PassTest desc (input, output)) = testCase desc $ do
    case (P.runPipeline P.defaultConfig) . f  =<< parseStr input of
          Left a -> assertFailure (show a)
          Right result -> assertEqual "Not equals" output $ (trimOutput . toString) result


passToTestgroup (P.PassDesc name func ts) = testGroup name $ map (testToCase func) ts

passesToGroups ps = map passToTestgroup (P.extractPasses ps)

tests = testGroup "Tests" $
  passesToGroups S.passes
  ++ passesToGroups C.passes
  ++ passesToGroups D.passes
  ++ passesToGroups I.passes
  ++ passesToGroups N.passes
  ++ passesToGroups R.passes
  ++
  [ 
    testGroup "Util"
    [ testCase "trimSpaces" $ trimSpaces "a b  c   d" @?= "a b c d"
    ]
  ]
