-- module Main where
module NofibConfig where

import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.Char
import Data.List.Split
import Options.Applicative
import System.FilePath ( (</>) )
import qualified System.Directory as Dir
import qualified System.Process as Proc

import NofibTargets

import Debug.Trace

-- TODO:
-- * clean
-- * tests (only subset of tests to run)
data Options = Options
    { optMeasurements :: [Measurement]
    , optGhc :: FilePath
    , optTag :: String
    , optGhcFlags :: String
    , optRtsFlags :: String
    , optNumCompiles :: !Int
    , optNumRuns :: !Int
    , optThreads :: !Int
    , optSpeed :: !Speed
    , optTests :: [FilePath]
    , optSkipChecks :: !Bool
    , optClean :: !Bool
    }
    deriving (Show)

data Measurement = Compilation | Runtime
    deriving (Eq, Read, Show)

data Speed
    = Fast
    | Norm
    | Slow
    deriving (Eq, Read, Show)


-- main = do
--     options <- parseFlags
--     putStrLn (show options)

buildDirectory :: String
buildDirectory = "_build"

outputPath :: Options -> FilePath
outputPath opt = buildDirectory </> optTag opt

dropOutput :: Options -> FilePath -> FilePath
dropOutput opt fp = drop (length (outputPath opt) + 1) fp

parseFlags :: IO Options
parseFlags = do
    defaultGhc <- getDefaultGhc
    userOptions <- execParser $
        info
            (parser defaultGhc <**> helper)
            (fullDesc <> progDesc "Print a greeting for TARGET" <>
             header "hello - a test for optparse-applicative")
    tag <- getTag userOptions
    tests <- resolveTests (optTests userOptions)
    return userOptions { optTag = tag, optTests = tests }

  where
    getTag options@(Options { optTag = optTag
                            , optGhc = optGhc
                            , optGhcFlags = optGhcFlags
                            })
        | not (null optTag) = return (optTag ++ dropSpace optGhcFlags)
        | otherwise = do
            (_, stdout, _) <- Proc.readProcessWithExitCode
                                  optGhc ["--version"] ""
            let ver =
                    takeWhile (\x -> isDigit x || x == '.') $
                    dropWhile (not . isDigit) stdout
            return $
                if null ver
                    then "unknown"
                    else ver ++ dropSpace optGhcFlags

    dropSpace = filter (not . isSpace)

    getDefaultGhc = do
        cwd <- Dir.getCurrentDirectory
        let stage2 = cwd </> ".." </> "inplace" </> "bin" </> "ghc-stage2"
        exists <- Dir.doesFileExist stage2
        return $ if exists then stage2 else "ghc"

parser :: String -> Parser Options
parser compiler =
    Options
        <$> option
                measurementReader
                (  long "measurement"
                <> short 'm'
                <> value [Compilation, Runtime]
                <> help ("What to measure: " ++ show [Compilation, Runtime])
                )
        <*> strOption (long "ghc" <> value compiler <> help "Path to GHC")
        <*> strOption
                (  long "tag"
                <> value ""
                <> help (  "Tag for the current run "
                        ++ "(GHC version ++ GHC flags by default)"))
        <*> strOption
                (  long "ghc_flags"
                <> value "-O2"
                <> help "GHC flags used for compilation")
        <*> strOption
                (  long "rts_flags"
                <> value ""
                <> help "RTS flags used when running benchmarks")
        <*> option auto
                (  long  "num_compiles"
                <> value 5
                <> help "Number of times to compile each module")
        <*> option auto
                (  long "num_runs"
                <> value 5
                <> help "Number of times to run each benchmark")
        <*> option auto
                (  long "threads"
                <> value 1
                <> help "Number of threads to use (ignored for measurements)")
        <*> option (eitherReader readCaseInsensitive)
                (  long "speed"
                <> value Norm
                <> help (  "How fast the benchmarks will run: "
                        ++ show [Fast, Norm, Slow]))
        <*> option
                filePathsReader
                (  long "tests"
                <> value []
                <> help "Tests to run")
        <*> switch
                (  long "skip_checks"
                <> help "Skip checking benchmark results")
        <*> switch
                (  long "clean"
                <> help "Remove the any old files")

measurementReader :: ReadM [Measurement]
measurementReader = eitherReader $ \s -> go (splitOn "," s) []
  where
    go [] ms = Right ms
    go (w:ws) ms
        | null w = go ws ms
        | otherwise =
            case readCaseInsensitive w of
                Right m -> go ws (m : ms)
                Left e -> Left e

    measurements = [Compilation, Runtime]

filePathsReader :: ReadM [FilePath]
filePathsReader = eitherReader (Right . splitOn ",")

readCaseInsensitive :: (Read a) => String -> Either String a
readCaseInsensitive s = eitherRead (upperFirst $ map toLower s)
  where
    upperFirst [] = []
    upperFirst (x : xs) = toUpper x : xs
    eitherRead s =
        case fmap fst (listToMaybe $ reads s) of
            Nothing -> Left "Parse error"
            Just a -> Right a
