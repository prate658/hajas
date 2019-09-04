{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( parseStr
  , deobfuscate
  , toString
  , passNames
  , P.Pipeline(..)
  , P.defaultConfig
  , P.Config(..)
  ) where

import Data.Semigroup ((<>))
import Debug.Trace
import Language.ECMAScript3
import Language.ECMAScript3.PrettyPrint
import Language.ECMAScript3.Syntax.Annotations

import qualified Constants as C
import qualified DeadCode as DC
import qualified Inlining as I
import qualified Normalization as N
import qualified Renaming as R
import qualified Simplification as S

import qualified Pass as P

-- avoiding code removal until the very end. However, it might allow even more simplifications
-- TODO repeat DC
passes =
  N.passes
  <> P.repeat (P.repeat (S.passes <> C.passes <> I.passes) <> DC.passes)
  <> R.passes
-- passes = C.passes
  -- N.passes <> P.repeat (S.passes <> C.passes <> I.passes) <> DC.passes <> R.passes

passNames :: [String]
passNames = concatMap (\i -> [P.passName i]) passes

deobfuscateTree :: P.Config -> JavaScript () -> Either String (JavaScript ())
deobfuscateTree config tree =
  let excludeFilter (P.PassDesc n _ _) = n `notElem` (P.excludes config)
      filtered = P.filterPasses excludeFilter passes
      pipeline = P.constructPipeline filtered
   in P.runPipeline config (pipeline tree)

deobfuscate :: String -> P.Config -> Either String String
deobfuscate str config = toString <$> (deobfuscateTree config =<< parseStr str)

toString :: JavaScript () -> String
toString = show . prettyPrint

parseStr :: String -> Either String (JavaScript ())
parseStr str =
  either (Left . show) (Right . removeAnnotations) (parseFromString str)
