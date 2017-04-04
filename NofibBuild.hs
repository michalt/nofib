{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.Maybe
import Data.List
import Data.Time.Clock
import qualified System.Directory as IO
import qualified System.IO as IO
import System.Exit
import System.IO
import System.Info
import System.Process

import qualified Data.ByteString as Bs
import qualified Data.Map as Map

import Development.Shake
import Development.Shake.Util
import Development.Shake.FilePath

import NofibConfig
import NofibTargets

import Debug.Trace
import GHC.Stack


-- TODO
-- * introduce a flag for the legacy format (and introduce the new one,
--   maybe .csv?)
-- * how do we run and configure tests?
--   - do we need more general way to configure things:
--     * parameters (e.g., files, flags)
--     * packages that are necessary to build (so that we can build or skip the
--       benchmarks as necessary)
--   - slow/normal/fast: do we need this? is anyone actually using this? maybe
--     we should recalibrate all benchmarks and remove the option? or restrict
--     to two values? (slow/fast) might be useful for things like blazing fast
--     i7 vs an ARM chip
--   - additional flags
-- * don't run parallel/ by default? can we use stack/cabal sandbox to get it
--   easily? (with arbitrary compiler?)
-- * create scripts/something to run the shake system (remember about the
--   idle-GC flag!!!)

main = do
    options <- parseFlags

    -- FIXME: print the config nicely
    putStrLn (show options)

    -- FIXME: Decide what to do with --clean. Should we nuke the whole _build or
    -- only the current tag? Or maybe introduce --clean_all?
    let outDir = outputPath options
    when (optClean options) (removeFiles outDir ["//"])
    outDirExists <- IO.doesDirectoryExist outDir
    when outDirExists
        (error $ "Output directory already exists: " ++ outDir
                  ++ " (use --clean if you want to remove it)")

    -- FIXME: only use threads for building if we do *not* measure compile time
    shake
        shakeOptions
        { shakeThreads = optThreads options
        , shakeFiles = buildDirectory </> optTag options ++ "/"
        , shakeReport = [buildDirectory </> optTag options </> "shake_report.html"]
        -- FIXME: Introduce flag for verbosity? Or maybe pass some flags to Shake?
        , shakeVerbosity = Development.Shake.Quiet
        } $
        buildRules options

getSrcDir :: Options -> FilePath -> FilePath
getSrcDir opt = takeDirectory . dropOutput opt

getSrcFile :: Options -> FilePath -> Action FilePath
getSrcFile options outputFile = do
    let file = dropOutput options outputFile
    filtered <- filterM doesFileExist [ file -<.> e | e <- ["hs", "lhs", "c"] ]
    case filtered of
        [f] -> return f
        [] -> error $ "Couldn't find source outputFile of " ++ outputFile
        fs ->
            error $
              "Found more than one source outputFile for " ++ outputFile ++
              ": " ++ show fs

configFileName :: FilePath
configFileName = "Main.cfg"

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs =  liftM concat (mapM f xs)


buildRules :: Options -> Rules ()
buildRules options@(Options {..}) = do
    let allMains =
                [ p </> "Main" <.> exe
                | t <- optTests, let p = outputPath options </> t
                ]
    want allMains

    when (Runtime `elem` optMeasurements) $ do
        want [outputPath options </> "nofib_results.txt"]

    -- FIXME: This currently only collects the statistics of running the
    -- benchmarks. What's still missing is to also print the stats from
    -- compiling them. Then we could use the standard output for reporting the
    -- what's the status of running the build.
    "_build//nofib_results.txt" %> \out -> do
        need allMains
        putNormal "Built all executables."

        forM_ optTests $ \srcDir -> do
            let outDir = outputPath options </> srcDir
                configFile = outDir </> configFileName
            need [configFile]
            config <- readTestVars' configFile
            srcDirFiles <- getDirectoryFiles srcDir ["*"]
            let (mstdin, stdouts, mstderr) =
                    getSrcStdFilenames config optSpeed srcDirFiles
            need $ maybe [] (\f -> [outDir </> "_stdin_expected" </> f]) mstdin
            need $ maybe [] (\f -> [outDir </> "_stderr_expected" </> f]) mstderr
            need $ map (\f -> outDir </> "_stdout_expected" </> f) stdouts
            runtimeFiles <- getRuntimeFiles srcDir
            need $ map (\f -> outDir </> "runtime_files" </> f) runtimeFiles

        putNormal "Copied all runtime files."

        results <- concatMapM (runTest options) optTests
        ghcStats <- flip concatMapM optTests $ \srcDir -> do
            statsFiles <- getDirectoryFiles
                              (outputPath options </> srcDir) ["*.o.stats_*", "*.stats_*"]
            let mkpath f = outputPath options </> srcDir </> f
                getStats f = do
                     let p = mkpath f
                     stats <- readFileLines p
                     let !_ = length stats -- Lazy I/O :(
                     return (nofibMsgHeader p : stats)
            concatMapM getStats statsFiles
        let finalSummary = formatSummary results
            resultLines =
                concatMap formatResult results ++
                ["", finalSummary]
        writeFileLines out (ghcStats ++ resultLines)
        putQuiet $ finalSummary
        putQuiet $ "Results available in: " ++ out

    "_build//Main.cfg" %> \out -> do

        let srcDir = getSrcDir options out
        mkfile <- readFile' $ srcDir </> "Makefile"
        -- Remove escaped newlines before calling `lines` to support, e.g.,
        -- multi-line flags.
        let collapse ('\\' : '\n' : rs) = ' ' : collapse rs
            collapse (c : cs) = c : collapse cs
            collapse [] = []
        writeFileLines out $ convertConfig (lines $ collapse mkfile)

    ["_build//Main" <.> exe, "_build//Main.stats_link", "_build//Main.stats_size"] &%>
            \[out, statsLink, statsSize] -> do
        let outDir = takeDirectory out
            srcDir = getSrcDir options out
            configFile = outDir </> configFileName
        srcDirFiles <- getDirectoryFiles srcDir ["*"]
        need [configFile]
        config <- readTestVars' configFile
        let srcFiles = getAllSrcFiles config srcDirFiles
            objFiles = map (\f -> outDir </> replaceExtension f "o") srcFiles
        need objFiles

        unit $ cmd optGhc $
            [ "+RTS", "-t" ++ statsLink, "-RTS", "-rtsopts", "-o", out]
                ++ objFiles
                ++ words optGhcFlags
                ++ words (config "HC_OPTS")
                ++ words (config "SRC_HC_OPTS")

        unit $ cmd "strip" [out]
        Stdout size <- cmd "size" [out]
        writeFile' statsSize size
        putQuiet $ "  HC [L] " ++ out

    ["_build//*.o", "_build//*.hi", "_build//*.o.stats_compile", "_build//*.o.stats_size"] &%>
            \[oFile, hiFile, statsCompile, statsSize] -> do
        let srcDir = getSrcDir options oFile
            outDir = takeDirectory oFile
        let depsFile = dropExtension oFile <.> "deps"
        need [depsFile]
        depsContents <- liftIO $ readFile depsFile
        let addOutput f
                | takeExtension f `elem` [".o", ".hi"] = outputPath options </> f
                | otherwise = f
            deps = map addOutput . concatMap snd $ parseMakefile depsContents
        need deps
        srcFiles <- getDirectoryFiles srcDir ["*"]
        file_name <- getSrcFile options oFile
        config <- readTestVars' $ outDir </> configFileName

        -- FIXME: Clean up the compiler invocations to have just *one* place
        -- that defines which flags to pass and how (e.g., the `config` stuff)
        unit $ cmd optGhc $
            [ "+RTS", "-t" ++ statsCompile, "-RTS"
            , "-c"
            , file_name
            , "-w"
            , "-o", oFile
            , "-ohi", hiFile
            , "-i" ++ outDir
            , "-odir=" ++ outDir
            , "-hidir=" ++ outDir
            ] ++
            words (config "HC_OPTS") ++
            words (config "SRC_HC_OPTS") ++
            words optGhcFlags

        Stdout size <- cmd "size" [oFile]
        writeFile' statsSize size
        putQuiet $ "  HC     " ++ oFile

    "_build//*.deps" %> \out -> do
        let srcDir = getSrcDir options out
            outDir = takeDirectory out
        srcFile <- getSrcFile options out
        config <- readTestVars' $ outDir </> configFileName
        unit $ cmd optGhc $
            [ "-w"
            , "-M"
            , srcFile
            , "-i" ++ srcDir
            , "-dep-suffix", ""
            , "-dep-makefile", out
            ] ++
            words (config "HC_OPTS") ++
            words (config "SRC_HC_OPTS")

    "_build//_stdin_expected/*" %> \out -> do
        let srcDir = dropLastPathElem (getSrcDir options out)
        copyFile' (srcDir </> takeFileName out) out

    "_build//_stderr_expected/*" %> \out -> do
        let srcDir = dropLastPathElem (getSrcDir options out)
        copyFile' (srcDir </> takeFileName out) out

    "_build//_stdout_expected/*" %> \out -> do
        let srcDir = dropLastPathElem (getSrcDir options out)
        copyFile' (srcDir </> takeFileName out) out

    "_build//runtime_files/*" %> \out -> do
        -- Copy the files that are necessary to run the benchmark.  The source
        -- directory should already contain `runtime_files` directory, so we
        -- just need to copy the files.
        copyFile' (dropOutput options out) out

dropLastPathElem :: FilePath -> FilePath
dropLastPathElem path =
    let elems = splitPath path
    in joinPath $ take (length elems - 1) elems

getSrcStdFilenames
    :: (String -> String)
    -> Speed
    -> [FilePath]
    -> (Maybe FilePath, [FilePath], Maybe String)
getSrcStdFilenames config speed srcDirFiles = (mstdin, stdouts, mstderr)
  where
    stdouts = filter (hasExtPrefix "stdout") srcDirFiles

    mstdin =
        case filter (hasExtPrefix "stdin") srcDirFiles of
            [] -- TODO(michalt): Consider getting rid of the STDIN_FILE
               -- option in Makefile in favor of .stdin suffix.
             ->
                case config "STDIN_FILE" of
                    "" -> Nothing
                    f -> Just f
            [f] -> Just $ f
            fs -> error $ "ERROR: Multiple .stdin files: " ++ show fs

    mstderr =
        case filter (hasExtPrefix "stderr") srcDirFiles of
            [] -> Nothing
            [f] -> Just f
            fs -> error $ "ERROR: Multiple .stderr files: " ++ show fs

    hasExtPrefix ext file =
        let fext = takeExtension file
        in ("." ++ ext) `isPrefixOf` fext ||
           ("." ++ map toLower (show speed) ++ ext) `isPrefixOf` fext

getRuntimeFiles :: FilePath -> Action [FilePath]
getRuntimeFiles srcDir = do
    let runtimeDir = srcDir </> "runtime_files"
    exists <- doesDirectoryExist runtimeDir
    if exists
        then getDirectoryFiles runtimeDir ["*"]
        else return []


-- | Returns the nofib message expected by `nofib-analyse`.
--
-- We assume that the provided @filePath@ is of the form
-- @<anything>/<test name>/<module name>.<ext>@
nofibMsgHeader :: FilePath -> String
nofibMsgHeader filePath =
    "==nofib== " ++ testName ++ ": " ++ todo ++ " " ++ modName ++ " follows..."
  where
    todo
        | ".stats_compile"  `isSuffixOf` filePath = "time to compile"
        | ".stats_link"     `isSuffixOf` filePath = "time to link"
        | ".stats_size"     `isSuffixOf` filePath = "size of"
        | otherwise                               = error $ "ERROR: unexpected file: " ++ filePath
    path = splitDirectories $ dropExtension filePath  -- drops only the .stats_* extension
    testAndMod = drop (length path - 2) path
    testName = head testAndMod
    modName = head $ drop 1 testAndMod


data BenchStatus
    = BenchSuccess
    | BenchBadExitCode
    | BenchBadStdout
    | BenchBadStderr
  deriving (Eq)

data BenchResult = BenchResult
    { brHeader :: String
    , brStats :: [String]
    , brStatus :: BenchStatus
    }

formatResult :: BenchResult -> [String]
formatResult results =
    brHeader results : brStats results ++ [formatStatus $ brStatus results]
  where
    formatStatus BenchSuccess = "All checks successfull."
    formatStatus BenchBadStderr = "FAILED: stderr mismatch!"
    formatStatus BenchBadStdout = "FAILED: stdout mismatch!"
    formatStatus BenchBadExitCode = "FAILED: wrong exit code!"

formatSummary :: [BenchResult] -> String
formatSummary results =
    let total = length results
        bad = length $ filter (\r -> brStatus r /= BenchSuccess) results
        t 1 = "1 test"
        t i = show i ++ " tests"
    in if bad == 0
        then "Ran " ++ t total ++ " successfully"
        else "WARNING: " ++ t bad ++ " failed, out of " ++ t total

-- | Run a test, checking stdout/stderr are as expected, and reporting time.
--   Return True if the test passes.
runTest :: Options -> FilePath -> Action [BenchResult]
runTest options@(Options {..}) test = do
    let testName = last (splitDirectories test)
        header = "==nofib== " ++ testName ++ ": time to run " ++ testName ++ " follows..."
        testDir = outputPath options </> test
    config <- liftIO $ readTestVars $ testDir </> "Main.cfg"
    let args =
            words (config "PROG_ARGS") ++
            words (config $ map toUpper (show optSpeed) ++ "_OPTS")
    (mstdinFile, stdoutFiles, stderrFiles) <- liftIO $ getStdFilenames testDir
    statsFile <- liftIO $ IO.canonicalizePath $ testDir </> "stat.txt"
    replicateM optNumRuns $ do
        let main = "./Main" <.> exe
            mainArgs = args ++ "+RTS" : words optRtsFlags ++ ["-t" ++ statsFile]
        exitCode <- liftIO $ runProcessForTest testDir main mainArgs mstdinFile

        actStdout <- liftIO $ Bs.readFile (testDir </> "_stdout_actual")
        actStderr <- liftIO $ Bs.readFile (testDir </> "_stderr_actual")
        expStdouts <- liftIO $ readExpectedStd stdoutFiles
        expStderrs <- liftIO $ readExpectedStd stderrFiles

        stats <- lines <$> liftIO (readFile statsFile)
        let status | optSkipChecks = BenchSuccess
                   | not (actStderr `elem` expStderrs) = BenchBadStderr
                   | not (actStdout `elem` expStdouts) = BenchBadStdout
                   | exitCode /= ExitSuccess = BenchBadExitCode
                   | otherwise = BenchSuccess

        putQuiet $ (if status == BenchSuccess then "  Bench  " else "  FAIL   ") ++ testDir
        return $ BenchResult header stats status

getStdFilenames :: FilePath -> IO (Maybe FilePath, [FilePath], [FilePath])
getStdFilenames testDir = do
    let listDir std = do
            let dir = testDir </> std
            exists <- IO.doesDirectoryExist dir
            if exists
                then map (dir </>) <$> IO.listDirectory dir
                else return []
    stdinList <- listDir "_stdin_expected"
    stderrs <- listDir "_stderr_expected"
    stdouts <- listDir "_stdout_expected"
    let stdin =
            case stdinList of
                [] -> Nothing
                [f] -> Just f
                fs -> error $ "ERROR: Multiple stdin files: " ++ show fs
    return (stdin, stdouts, stderrs)


readExpectedStd :: [FilePath] -> IO [Bs.ByteString]
readExpectedStd [] = return [Bs.empty]
readExpectedStd files = mapM Bs.readFile files

runProcessForTest :: (HasCallStack) => FilePath -> String -> [String] -> Maybe FilePath -> IO ExitCode
runProcessForTest testDir cmd args mstdin = do
    stdinS <- case mstdin of
                  Nothing -> UseHandle <$> openBinaryFile "/dev/null" ReadMode
                  Just f -> UseHandle <$> openBinaryFile f ReadMode
    stdoutH <- openBinaryFile (testDir </> "_stdout_actual") WriteMode
    stderrH <- openBinaryFile (testDir </> "_stderr_actual") WriteMode
    let process = (proc cmd args)
                      { std_in = stdinS
                      , std_out = UseHandle stdoutH
                      , std_err = UseHandle stderrH
                      , cwd = Just testDir
                      }
    (_, _, _, procH) <- createProcess process
    waitForProcess procH

-- | Given the source of a Makefile, slurp out the configuration strings.
convertConfig :: [String] -> [String]
convertConfig xs =
    [remap a ++ " = " ++ b | x <- xs, let (a, b) = separate x, a `elem` keep]
  where
    keep =
        [ "PROG_ARGS"
        , "EXCLUDED_SRCS"
        , "HS_SRCS"
        , "SRCS"
        , "HC_OPTS"
        , "SRC_HC_OPTS"
        , "SRC_RUNTEST_OPTS"
        , "SLOW_OPTS"
        , "NORM_OPTS"
        , "FAST_OPTS"
        , "STDIN_FILE"
        ]
    remap "SRC_RUNTEST_OPTS" = "PROG_ARGS"
    remap x = x
    separate x = (name, rest)
      where
        (name, x2) = span (\x -> isAlpha x || x == '_') x
        -- NOTE: This should correctly handle things like:
        -- `SRC_HC_OPTS += +RTS ...`
        rest = dropWhile isSpace $ dropWhile (`elem` "+=") $ dropWhile isSpace x2

-- | Read a configuration file (new format) into a function supplying options.
readTestVars :: FilePath -> IO (String -> String)
readTestVars x = do
    src <- readFile x
    let keyValues =
            [ ( concat key, drop 1 value)
            | line <- lines src
            , let (key, value) = break (== "=") (words line)
            ]
        res = Map.map (intercalate " ") $ Map.fromListWith (++) keyValues
    return $ \k -> fromMaybe "" (Map.lookup k res)

-- | readTestVars lifted into the Action monad.
readTestVars' :: FilePath -> Action (String -> String)
readTestVars' x = do
    need [x]
    liftIO $ readTestVars x


-- | Like the standard removeDirectoryRecursive, but doesn't fail if the path is missing.
removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive x = do
    b <- IO.doesDirectoryExist x
    when b $ IO.removeDirectoryRecursive x


processForTest :: FilePath -> String -> [String] -> CreateProcess
processForTest cwd cmd args = (proc cmd args) { cwd = Just cwd }

isHaskellFile :: FilePath -> Bool
isHaskellFile f = takeExtension f `elem` [".hs", ".lhs"]

getAllSrcFiles :: (String -> String) -> [FilePath] -> [FilePath]
getAllSrcFiles config srcDirFiles =
    let srcFiles =
            case config "HS_SRCS" <|> config "SRCS" of
                "" -> filter isHaskellFile srcDirFiles
                ns -> words ns
    in case config "EXCLUDED_SRCS" of
           "" -> srcFiles
           excluded -> srcFiles \\ words excluded
