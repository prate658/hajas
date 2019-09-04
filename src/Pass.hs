{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Pass
  ( passify
  , Pass(..)
  , Config(..)
  , defaultConfig
  , PassDesc(..)
  , Pipeline(..)
  , PassTest(..)
  , passTest
  , constructPipeline
  , extractPasses
  , filterPasses
  , runPipeline
  , Pass.pass
  , Pass.repeat
  ) where

import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Semigroup
import Debug.Trace
import Language.ECMAScript3
import Data.Maybe
import qualified Data.Map as M

type Change = [String]

newtype Pass a = Pass
  { runPass :: ExceptT String (WriterT Change (Reader Config)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadWriter Change
             , MonadError String
             , MonadReader Config
             )

type PassFunc = JavaScript () -> Pass (JavaScript ())

instance Show PassFunc where
  show _ = "<PassFunc>"

data PassDesc = PassDesc
  { passName :: String
  , passFunc :: PassFunc
  , tests :: [PassTest]
  } deriving (Show)

instance Eq PassDesc where
  (==) a b = passName a == passName b

data PassTest = PassTest
  { description :: String
  , test :: (String, String)
  } deriving (Show, Eq)

data Pipeline a
  = PipeOne a
  | PipeSeq [Pipeline a]
  | PipeRep (Pipeline a)
  | PipeEmpty
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Monoid (Pipeline p) where
  mempty = PipeEmpty
  mappend PipeEmpty a = a
  mappend a PipeEmpty = a
  mappend a@PipeOne {} b@PipeOne {} = PipeSeq [a, b]
  mappend (PipeSeq l) (PipeSeq l2) = PipeSeq (l ++ l2)
  mappend (PipeSeq l) a = PipeSeq (l ++ [a])
  mappend a@PipeOne {} (PipeSeq l) = PipeSeq (a : l)
  mappend a@PipeRep {} b = PipeSeq [a, b]
  mappend b a@PipeRep {} = PipeSeq [b, a]

instance Semigroup (Pipeline a) where
    (<>) = mappend

data Config = Config
  { excludes :: [String]
  , replacements :: Maybe (M.Map String String)
  } deriving (Eq, Show)

defaultConfig = Config [] Nothing

pass :: String -> PassFunc -> [PassTest] -> Pipeline PassDesc
pass d f ts = PipeOne (PassDesc d f ts)

passTest :: String -> String -> String -> PassTest
passTest desc value expected = PassTest desc (value, expected)

repeat :: Pipeline a -> Pipeline a
repeat = PipeRep

passify :: (JavaScript () -> JavaScript ()) -> PassFunc
passify f = return . f

constructPipeline :: Pipeline PassDesc -> PassFunc
constructPipeline (PipeOne a) = passFunc a
constructPipeline (PipeRep p) = repeatUntilStable (constructPipeline p)
constructPipeline (PipeSeq ps) = foldr ((>=>) . constructPipeline) return ps
constructPipeline PipeEmpty = return

extractPasses :: Pipeline PassDesc -> [PassDesc]
extractPasses = foldr (:) []

repeatUntilStable :: PassFunc -> PassFunc
repeatUntilStable f tree = do
  old <- f tree
  new <- f old
  if old == new
    then return new
    else repeatUntilStable f new

runPipeline :: Config -> Pass (JavaScript ()) -> Either String (JavaScript ())
runPipeline config = (fst . (flip runReader config) . runWriterT . runExceptT . runPass)

-- TODO refactor...
filterPasses :: (Eq a) => (a -> Bool) -> Pipeline a -> Pipeline a
filterPasses f o@(PipeOne d) =
  if f d
    then o
    else PipeEmpty
filterPasses f (PipeSeq ps) =
  let pruned = filter (/= PipeEmpty) (map (filterPasses f) ps)
   in if null pruned
        then PipeEmpty
        else PipeSeq pruned
filterPasses f (PipeRep p) = PipeRep (filterPasses f p)
filterPasses _ PipeEmpty = PipeEmpty
