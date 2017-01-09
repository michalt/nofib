module NofibTargets where

import Control.Monad
import Data.List
import System.FilePath
import qualified System.Directory as IO

import Debug.Trace

testRoots :: [FilePath]
testRoots =
    [ "imaginary"
    -- , "gc"
    -- TODO(michalt): Figure out if and how `gc` is used. Currently the
    -- following benchmarks are broken:
    -- * fulsom
    -- * gc_bench
    -- * happy
    -- * hash
    -- * mutstore1
    -- * mutstore2
    --
    -- , "parallel"
    -- TODO(michalt): Everything seems to require `parallel` package (maybe we
    -- should check it in along with benchmarks?). But even with `parallel`
    -- there are a bunch of broken benchmarks:
    -- * gray requires parsec
    -- * mandel
    -- * matmult
    -- * minimax
    -- * nbody
    -- * prsa
    -- * sumeuler
    -- * transclos
    , "real"
    , "shootout"
    , "spectral"
    , "spectral" </> "hartel"
    ]

-- | These are tests that are under testRoots, but should be skipped (all are
-- skipped by the Makefile system)
disabledTests :: [String]
disabledTests =
    [ "hartel" -- since it's a test root itself
    -- Add here any benchmarks that should be disabled.
    ]

-- | Directories containing tests that the system can run, excluding the
-- @disabledTests@.
allTests :: IO [FilePath]
allTests = do
    tests <-
        forM testRoots $ \root -> do
            contents <- IO.listDirectory root
            return [ root </> t | t <- contents , t `notElem` disabledTests ]
    fmap sort $
        flip filterM (concat tests) $ \test -> do
            b <- IO.doesDirectoryExist test
            if not b
                then return False
                else IO.doesFileExist $ test </> "Makefile"

-- | Given the tests the user asked for, expand them out, e.g., @real@ is the
-- full @real@ suite.
resolveTests :: [String] -> IO [FilePath]
resolveTests [] = allTests
resolveTests prefixes = do
    tests <- allTests
    let isDirPrefix t p = splitDirectories p `isPrefixOf` splitDirectories t
        filtered = filter (\t -> any (isDirPrefix t) prefixes) tests
    when (null filtered) $
        error $ "Could not find any tests to run based on: " ++ unwords prefixes
    return filtered

