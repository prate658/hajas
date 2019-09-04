{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Environment
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad
import Data.List.Split (splitOn)
import qualified Data.Map as M

import Lib

data OutputMode = OutputNormal | OutputListPasses | OutputAST
  deriving (Eq, Show)

data Options = Options
  { optMode :: OutputMode
  , optInputPath :: Maybe String
  , optReplacementFile :: Maybe String
  , optExcludes :: [String]
  }

configParse :: Parser Options
configParse = Options
  <$> (flag OutputNormal OutputAST (short 'a' <> help "Print AST")
       <|> flag' OutputListPasses (short 'l' <> help "List passes"))
  <*> (optional $ strOption (
          short 'f'
          <> metavar "FILE"
          <> help "Input file. stdin if not given"))
  <*> (optional $ strOption (
          short 'r'
          <> metavar "FILE"
          <> help "Replacements file"))
  <*> many (strOption (short 'e'
                       <> metavar "PASS"
                       <> help "Exclude pass"))

getInput :: Options -> IO String
getInput Options{..}  = maybe getContents readFile optInputPath

runner :: Options -> IO ()
runner (Options OutputListPasses _ _ _)
  = putStrLn "Available passes:" >> forM_ passNames putStrLn

runner conf@(Options OutputAST _ _ _)
  = (show . parseStr) <$> getInput conf >>= print

runner conf@(Options OutputNormal _ repl excludes') = do
    config <- optionsToConf conf
    deobfuscate' config <$> getInput conf >>= printOutput
      where
        deobfuscate' = flip deobfuscate
        printOutput out = putStrLn $ either ("Parsing error occurred " ++) id out

optionsToConf :: Options -> IO Config
optionsToConf Options{..} = do
    repls <- parseReplacementsFile optReplacementFile
    return $ defaultConfig {excludes = optExcludes, replacements = repls}

    where
        parseReplacementsFile :: Maybe String -> IO (Maybe (M.Map String String))
        parseReplacementsFile (Just filepath) = Just . parseReplacements <$> readFile filepath 
        parseReplacementsFile Nothing         = return Nothing

        parseReplacements :: String -> M.Map String String
        parseReplacements s = M.fromList $ map (tuplify . splitOn ";") (lines s)

        tuplify [x, y] = (x, y)
        tuplify _ = error "tuplify: invalid number of elements"

main :: IO ()
main = runner =<< execParser opts
  where
    opts = info (configParse <**> helper)
      (fullDesc <> progDesc "JavaScript deobfuscator")
