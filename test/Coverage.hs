{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Description: Coverage measurement and baseline checks for all library modules
--
-- This tool runs separately from the test suite.
-- doc/qa.md and .github/workflows/haskell.yml give the commands and package versions.
-- The tool must use the Cabal version that configured the build.
--
-- > runghc <package flags> test/Coverage.hs check  <build-dir> test/coverage-baseline.tsv <report-dir>
-- > runghc <package flags> test/Coverage.hs update <build-dir> test/coverage-baseline.tsv <report-dir>
-- > runghc <package flags> test/Coverage.hs self-test
--
-- The tool reads all library module names from the Cabal package description.
-- It includes the private @reflex-vty-internal@ library.
-- It finds @.mix@ files and the parent @.tix@ file only in the specified build directory.
-- It adds child tick counts from @REFLEX_VTY_COVERAGE_CHILD_DIR@ to the parent counts.
-- Compiled modules without tick counts receive zero counts.
-- The report gives covered and total counts for expressions, boolean pairs, and alternatives.
-- @check@ uses integer cross multiplication to compare coverage with the committed baseline.
-- @update@ replaces the local baseline but cannot run when @CI=true@.
-- @self-test@ checks the calculations with synthetic Mix/Tix data in temporary directories.
module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, unless, when)
import Data.Char (isDigit, isLower, isSpace)
import Data.List (intercalate, isPrefixOf, isSuffixOf, nub, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime, addUTCTime)
import Distribution.ModuleName (components)
import Distribution.PackageDescription
  ( Library (..)
  , PackageDescription (..)
  , otherModules
  )
import Distribution.PackageDescription.Configuration
  ( flattenPackageDescription
  )
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Compiler (OptimisationLevel (..), compilerId)
import Distribution.Simple.Configure (getPersistBuildConfig)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.LibraryName (libraryNameString)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo (..))
import Distribution.Utils.Path (makeSymbolicPath)
import Distribution.Verbosity (silent)
import System.Directory
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath
import System.IO
import System.IO.Extra (withTempDir)
import System.Process (rawSystem, readProcess)
import Trace.Hpc.Mix
import Trace.Hpc.Tix
import Trace.Hpc.Util (Hash, HpcPos, toHpcPos)

-- | The supported compiler, optimization, and schema versions.
-- Report the required commands if these settings differ.
expectedGhc :: String
expectedGhc = "9.12.3"

expectedOpt :: String
expectedOpt = "0"

schemaVersion :: String
schemaVersion = "1"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  getArgs >>= \case
    ["self-test"] -> selfTest
    ["check", buildDir, baseline, reportDir] -> do
      current <- analyzeEverything buildDir
      baselineRows <- readBaseline baseline
      let problems = compareWithBaseline baselineRows current
      writeReports reportDir current
      if null problems
        then putStrLn "coverage check: OK"
        else do
          putStrLn "coverage check: FAILED"
          mapM_ (putStrLn . ("  - " ++)) problems
          exitWith (ExitFailure 1)
    ["update", buildDir, baseline, reportDir] -> do
      ci <- lookupEnv "CI"
      when (ci == Just "true") $ do
        putStrLn "coverage update: forbidden under CI=true"
        exitWith (ExitFailure 1)
      current <- analyzeEverything buildDir
      writeReports reportDir current
      writeBaseline baseline current
      putStrLn ("coverage update: wrote " ++ baseline)
    _ -> do
      putStrLn "usage: Coverage.hs check|update <build-dir> <baseline.tsv> <report-dir>"
      putStrLn "       Coverage.hs self-test"
      exitWith (ExitFailure 2)

-- | A tix module's declared tick count.
tixLen :: TixModule -> Int
tixLen (TixModule _ _ n _) = n

-- | The modules in a tix file.
theTixModules :: Tix -> [TixModule]
theTixModules (Tix xs) = xs

-- | Fail with a clear message.
die :: String -> IO a
die msg = hPutStrLn stderr ("coverage: " ++ msg) >> exitWith (ExitFailure 1)

----------------------------------------------------------------------------
-- Package inventory
----------------------------------------------------------------------------

data ComponentKind = CompMain | CompInternal
  deriving (Eq, Ord, Show)

componentName :: ComponentKind -> String
componentName CompMain = "main"
componentName CompInternal = "internal"

-- | Get each library module and its component from the Cabal package description.
-- Include all conditional branches in the module list.
-- The coverage build must compile each module in this list.
-- @packageDescription gpd@ alone does not include modules in conditional branches.
-- Exclude generated @Paths_@ modules.
inventoryModules :: IO [(ComponentKind, String)]
inventoryModules = do
  gpd <- readGenericPackageDescription silent Nothing (makeSymbolicPath "reflex-vty.cabal")
  let pd = flattenPackageDescription gpd
      dotted = intercalate "." . components
      modsOf lib = nub (map dotted (exposedModules lib ++ otherModules (libBuildInfo lib)))
      mainMods = maybe [] modsOf (library pd)
      subs =
        [ (fromMaybe "sub" (libraryNameString (libName sub)), modsOf sub)
        | sub <- subLibraries pd
        ]
      internalNames = ["reflex-vty-internal"]
      internalMods = concat [ms | (n, ms) <- subs, n `elem` internalNames]
  when (null internalMods) $
    die "internal sublibrary reflex-vty-internal not found in reflex-vty.cabal"
  let classify m = if m `elem` internalMods then CompInternal else CompMain
      modsWithComp =
        [ (classify m, m)
        | m <- nub (mainMods ++ concat (map snd subs))
        , not ("Paths_" `isPrefixOf` m)
        ]
  pure (sortOn (\(c, m) -> (componentName c, m)) modsWithComp)

----------------------------------------------------------------------------
-- Analysis
----------------------------------------------------------------------------

data ModuleCounts = ModuleCounts
  { mcExpressions :: (Int, Int)
  -- ^ (covered, total) expression boxes
  , mcBooleans :: (Int, Int)
  -- ^ (both-branches-taken pairs, total boolean pairs)
  , mcAlternatives :: (Int, Int)
  -- ^ (covered, total) alternative boxes
  }
  deriving (Eq, Show)

data ModuleResult = ModuleResult
  { mrComponent :: ComponentKind
  , mrModule :: String
  , mrCounts :: ModuleCounts
  , mrHash :: Hash
  , mrTicks :: [Integer]
  , mrMixPath :: FilePath
  -- ^ The source .mix file. The report uses a copy of this file.
  }

-- | Discover mix and tix files under the build dir and analyze.
analyzeEverything :: FilePath -> IO [ModuleResult]
analyzeEverything buildDir = do
  mods <- inventoryModules
  when (null mods) $ die "package inventory is empty"
  validateBuildSettings buildDir
  mixFiles <- filter (hasComponents ["hpc", "vanilla", "mix"]) <$> findWithSuffix buildDir ".mix"
  parentTix0 <-
    filter (\p -> hasComponents ["t", "reflex-vty-test"] p && hasComponents ["hpc", "vanilla", "tix"] p)
      <$> findWithSuffix buildDir ".tix"
  childDir <- lookupEnv "REFLEX_VTY_COVERAGE_CHILD_DIR"
  -- Convert relative paths to normalised absolute paths before comparison.
  -- This prevents incorrect matches caused by ".", "..", or different base directories.
  childDirAbs <- traverse (\d -> normalise <$> makeAbsolute d) childDir
  -- Use an empty child directory for each run.
  -- Create the run-start marker before the suite starts.
  -- Each child writes a signal-<mode>.tix file after the marker and before the parent tix file.
  -- Analysis does not change this directory. Thus, one run can supply data for repeated checks and updates.
  (childMarkerTime, childTix) <- case childDir of
    Nothing -> pure (Nothing, [])
    Just dir -> do
      exists <- doesDirectoryExist dir
      unless exists $
        die ("REFLEX_VTY_COVERAGE_CHILD_DIR=" ++ dir ++ " does not exist")
      let marker = dir </> runStartName
      markerExists <- doesFileExist marker
      unless markerExists $
        die
          ( "run-start marker "
              ++ marker
              ++ " is missing. Start each run with an empty child directory."
              ++ " Create the marker before the instrumented run:\n"
              ++ "  rm -rf "
              ++ dir
              ++ " && mkdir -p "
              ++ dir
              ++ " && touch "
              ++ marker
          )
      markerTime <- getModificationTime marker
      entries <- filter (/= runStartName) . sort <$> listDirectory dir
      tix <- forM entries $ \e -> do
        let f = dir </> e
        isDir <- doesDirectoryExist f
        if isDir || not (".tix" `isSuffixOf` e)
          then die ("unexplained file in child tix directory: " ++ f)
          else do
            unless (knownSignalTixName e) $
              die
                ( "unknown child tix name "
                    ++ e
                    ++ " (expected signal-<mode>.tix from the signal child harness)"
                )
            pure f
      pure (Just markerTime, tix)
  parentPairs <-
    traverse (\f -> (,) f . normalise <$> makeAbsolute f) parentTix0
  let parentTix =
        [ f
        | (f, fa) <- parentPairs
        , not (maybe False (isUnderDir fa) childDirAbs)
        ]
  case (childMarkerTime, parentTix) of
    (Just markerTime, [p]) -> do
      parentTime <- getModificationTime p
      forM_ childTix $ \c -> do
        t <- getModificationTime c
        case childTimeProblem markerTime parentTime c t of
          Just problem -> die problem
          Nothing -> pure ()
    _ -> pure () -- analyze reports missing or multiple parent files.
  analyze buildDir mods mixFiles parentTix childTix

-- | The marker file that the run creates before the suite starts.
runStartName :: String
runStartName = "run-start"

-- | Accept @signal-<mode>.tix@ names from the signal test children.
-- Reject other names in the child directory.
knownSignalTixName :: String -> Bool
knownSignalTixName name =
  "signal-" `isPrefixOf` name
    && ".tix" `isSuffixOf` name
    && not (null mode)
    && all okMode mode
  where
    prefix = "signal-" :: String
    suffix = ".tix" :: String
    mode = drop (length prefix) (take (length name - length suffix) name)
    okMode c = isLower c || isDigit c || c == '-'

-- | Check the modification time of a child tix file.
-- Return 'Nothing' if the time is between the marker time and the parent tix time, inclusive.
-- Otherwise, return an error message.
childTimeProblem :: UTCTime -> UTCTime -> FilePath -> UTCTime -> Maybe String
childTimeProblem markerTime parentTime path t
  | t < markerTime =
      Just
        ( "child tix "
            ++ path
            ++ " is older than the run-start marker."
            ++ " Use only files from this run."
        )
  | t > parentTime =
      Just
        ( "child tix "
            ++ path
            ++ " is newer than parent tix."
            ++ " Each child must stop before the parent writes its tix file."
            ++ " Use only files from this run."
        )
  | otherwise = Nothing

-- | Check each saved Cabal build configuration in the build directory.
-- Each configuration must specify the required GHC version and no optimization.
-- HPC files do not record these settings.
-- The tool must use the Cabal version that wrote @setup-config@.
-- Stop if the tool cannot read a configuration.
validateBuildSettings :: FilePath -> IO ()
validateBuildSettings buildDir = do
  ghcDirNames <- nub . map takeFileName <$> findMatching (isGhcDirName . takeFileName) buildDir
  case ghcDirNames of
    [n] | n == "ghc-" ++ expectedGhc -> pure ()
    [] -> die ("no ghc-" ++ expectedGhc ++ " artifacts under " ++ buildDir)
    ds ->
      die
        ( "ambiguous compiler directories under "
            ++ buildDir
            ++ ": "
            ++ show ds
            ++ "; the coverage build dir must contain exactly one compiler's artifacts"
        )
  setupConfigs <- sort <$> findMatching ((== "setup-config") . takeFileName) buildDir
  case setupConfigs of
    [] ->
      die
        ( "no setup-config under "
            ++ buildDir
            ++ ". Use the Cabal build directory from the instrumented run."
        )
    cfgs -> do
      results <-
        forM cfgs $ \cfg ->
          (,) cfg
            <$> try
              ( getPersistBuildConfig
                  (Just (makeSymbolicPath "."))
                  (makeSymbolicPath (takeDirectory cfg))
              )
      let readable = [(c, lbi) | (c, Right lbi) <- results]
          unreadable = [c | (c, Left (_ :: SomeException)) <- results]
          firstUnreadable = case unreadable of
            (c : _) -> Just c
            [] -> Nothing
      unless (null unreadable) $
        die
          ( "cannot read saved build configurations under "
              ++ buildDir
              ++ " ("
              ++ show (length unreadable)
              ++ " config(s)"
              ++ maybe "" ("; first: " ++) firstUnreadable
              ++ "). Use the Cabal library version that configured the build directory."
          )
      forM_ readable $ \(cfg, lbi) -> do
        let cid = prettyShow (compilerId (compiler lbi))
        when (cid /= "ghc-" ++ expectedGhc) $
          die
            ( cfg
                ++ " was configured with "
                ++ cid
                ++ ", but the baseline requires ghc-"
                ++ expectedGhc
                ++ ". Use the required settings:\n  "
                ++ canonicalCommand
            )
        let opt = withOptimization lbi
        when (opt /= NoOptimisation) $
          die
            ( cfg
                ++ " was configured with optimization "
                ++ show opt
                ++ ", but the baseline requires optimization "
                ++ expectedOpt
                ++ ". Use the required settings:\n  "
                ++ canonicalCommand
            )
  mixFiles <- filter (hasComponents ["hpc", "vanilla", "mix"]) <$> findWithSuffix buildDir ".mix"
  when (null mixFiles) $ die "no vanilla instrumented mix artifacts found"
  where
    isGhcDirName n =
      "ghc-" `isPrefixOf` n
        && not (null (drop 4 n))
        && all (\c -> isDigit c || c == '.') (drop 4 n)

-- | Match consecutive path components, not a text prefix.
hasComponents :: [FilePath] -> FilePath -> Bool
hasComponents wanted = go . splitDirectories
  where
    go [] = False
    go parts@(_ : rest) = wanted `isPrefixOf` parts || go rest

-- | Whether @path@ lies at or under directory @dir@. Both arguments must be
-- normalised and absolute (see 'analyzeEverything').
isUnderDir :: FilePath -> FilePath -> Bool
isUnderDir path dir = splitDirectories dir `isPrefixOf` splitDirectories path

-- | Search @root@ and its subdirectories for paths that satisfy the predicate.
findMatching :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findMatching p root = go root
  where
    go dir = do
      entries <- listDirectory dir
      fmap concat . forM (sort entries) $ \e -> do
        let f = dir </> e
        isDir <- doesDirectoryExist f
        if isDir
          then do
            rest <- go f
            pure ([f | p f] ++ rest)
          else pure [f | p f]

-- | Search for a suffix only in the root directory and its subdirectories.
findWithSuffix :: FilePath -> String -> IO [FilePath]
findWithSuffix root suffix = findMatching (suffix `isSuffixOf`) root

-- | Analyze the specified files. Tests can supply synthetic files.
analyze
  :: FilePath
  -> [(ComponentKind, String)]
  -> [FilePath]
  -> [FilePath]
  -> [FilePath]
  -> IO [ModuleResult]
analyze _buildDir mods mixFiles parentTix childTix = do
  parent <- case nub parentTix of
    [] -> die ("no .tix found under " ++ _buildDir ++ "; run the instrumented suite first")
    [path] -> readTheTix path
    paths -> die ("ambiguous parent tix candidates: " ++ show paths)
  children <- mapM readTheTix childTix
  let byModule =
        Map.fromListWith
          (\a b -> a ++ b)
          [(dropExtension (takeFileName f), [f]) | f <- mixFiles]
      tixByName =
        Map.fromListWith
          (\a b -> a ++ b)
          [ (tixModuleName tm, [tm])
          | t <- parent : children
          , tm <- theTixModules t
          ]
  forM mods $ \(comp, m) -> do
    mixPath <- case Map.lookup m byModule of
      Nothing -> pure Nothing
      Just [f] -> pure (Just f)
      Just fs -> die ("ambiguous mix files for " ++ m ++ ": " ++ show fs)
    case mixPath of
      Nothing ->
        -- A missing module would reduce the denominator.
        -- Report an error instead of omitting the module.
        die ("missing mix file for " ++ m ++ " under " ++ _buildDir)
      Just mixPath' -> do
        (mixHash', labels) <- readMixFile mixPath' m
        let qualifiedName = takeFileName (takeDirectory mixPath') ++ "/" ++ m
            tms = Map.findWithDefault [] qualifiedName tixByName ++ Map.findWithDefault [] m tixByName
        ticks <- case tms of
          [] -> pure (replicate (length labels) 0)
          (tm0 : _) -> do
            let hashes = map tixModuleHash tms
                lengths = map tixLen tms
            when (length (nub (map show hashes)) > 1) $
              die ("tick hash mismatch for " ++ m)
            when (length (nub lengths) > 1) $
              die ("tick length mismatch for " ++ m)
            when (tixModuleHash tm0 /= mixHash') $
              die ("tix hash does not match mix for " ++ m)
            when (tixLen tm0 /= length labels) $
              die
                ( "tick length "
                    ++ show (tixLen tm0)
                    ++ " does not match mix box count "
                    ++ show (length labels)
                    ++ " for "
                    ++ m
                )
            pure (foldl1 (zipWith (+)) (map tixModuleTixs tms))
        pure
          ModuleResult
            { mrComponent = comp
            , mrModule = m
            , mrCounts = classifyBoxes labels ticks
            , mrHash = mixHash'
            , mrTicks = ticks
            , mrMixPath = mixPath'
            }
  where
    readTheTix p =
      readTix p >>= \case
        Nothing -> die ("malformed tix " ++ p)
        Just t -> do
          forM_ (theTixModules t) $ \tm ->
            when (tixLen tm /= length (tixModuleTixs tm)) $
              die
                ( "tix "
                    ++ p
                    ++ ": module "
                    ++ tixModuleName tm
                    ++ " declares "
                    ++ show (tixLen tm)
                    ++ " ticks but carries "
                    ++ show (length (tixModuleTixs tm))
                )
          pure t

readMixFile :: FilePath -> String -> IO (Hash, [BoxLabel])
readMixFile mixPath m = do
  mix <- readMix [takeDirectory mixPath] (Left m)
  case mix of
    Mix _ _ h _ entries -> pure (h, map snd entries)

-- | Split box labels into the three metrics with their tick values.
classifyBoxes :: [BoxLabel] -> [Integer] -> ModuleCounts
classifyBoxes labels ticks =
  let pairs = zip labels ticks
      exprs =
        ( length [() | (ExpBox _, t) <- pairs, t > 0]
        , length [() | (ExpBox _, _) <- pairs]
        )
      bools = boolCounts pairs
      alts =
        ( length [() | (b, t) <- pairs, isAlt b, t > 0]
        , length [() | (b, _) <- pairs, isAlt b]
        )
  in ModuleCounts exprs bools alts
  where
    isAlt (ExpBox True) = True
    isAlt _ = False
    -- HPC uses adjacent True and False boxes for each boolean condition.
    -- Both boxes must have the same constructor.
    -- Do not pair boxes from different conditions.
    boolCounts :: [(BoxLabel, Integer)] -> (Int, Int)
    boolCounts = go (0, 0)
      where
        go acc [] = acc
        go (cov, tot) ((BinBox c True, t1) : (BinBox c' False, t2) : rest)
          | c == c' =
              go (cov + (if t1 > 0 && t2 > 0 then 1 else 0), tot + 1) rest
        go acc (_ : rest) = go acc rest

----------------------------------------------------------------------------
-- Baseline
----------------------------------------------------------------------------

data BaselineRow = BaselineRow
  { brComponent :: String
  , brModule :: String
  , brCounts :: ModuleCounts
  }
  deriving (Eq, Show)

baselineHeader :: String
baselineHeader =
  "# reflex-vty coverage baseline\tschema="
    ++ schemaVersion
    ++ "\tghc="
    ++ expectedGhc
    ++ "\topt="
    ++ expectedOpt

canonicalCommand :: String
canonicalCommand =
  unlines
    [ "Use the coverage job in .github/workflows/haskell.yml."
    , "Use GHC 9.12.3, Cabal 3.16.1.0, and a new dist-coverage build."
    , "Install the matching Cabal library in a separate package environment. See doc/qa.md."
    , "export REFLEX_VTY_COVERAGE_CHILD_DIR=\"$PWD/dist-coverage/child-tix\""
    , "mkdir -p \"$REFLEX_VTY_COVERAGE_CHILD_DIR\""
    , "touch \"$REFLEX_VTY_COVERAGE_CHILD_DIR/run-start\""
    , "REFLEX_VTY_GOLDEN_UPDATE=0 cabal test reflex-vty-test --builddir=dist-coverage --disable-optimization --enable-coverage --test-show-details=direct"
    , "Then run Coverage.hs check using the matching package environment."
    ]

readBaseline :: FilePath -> IO [BaselineRow]
readBaseline path = do
  exists <- doesFileExist path
  unless exists $ die ("missing baseline " ++ path)
  ls <- lines <$> readFile path
  case ls of
    (h : rows) -> do
      unless (h == baselineHeader) $
        die
          ( "baseline header mismatch (this tool only compares ticks from GHC "
              ++ expectedGhc
              ++ " at optimization "
              ++ expectedOpt
              ++ "); regenerate canonically:\n  "
              ++ canonicalCommand
          )
      mapM parseRow rows
    [] -> die ("malformed baseline " ++ path ++ ": no header")
  where
    parseRow row = case splitTab row of
      [comp, m, ec, et, bc, bt, ac, at] -> do
        ec' <- readI ec
        et' <- readI et
        bc' <- readI bc
        bt' <- readI bt
        ac' <- readI ac
        at' <- readI at
        pure (BaselineRow comp m (ModuleCounts (ec', et') (bc', bt') (ac', at')))
      _ -> die ("malformed baseline row: " ++ show row)
    readI s = case reads s of
      [(n, "")] -> pure (n :: Int)
      _ -> die ("non-integer in baseline: " ++ s)

splitTab :: String -> [String]
splitTab s = case break (== '\t') s of
  (a, '\t' : rest) -> a : splitTab rest
  (a, _) -> [a]

writeBaseline :: FilePath -> [ModuleResult] -> IO ()
writeBaseline path results = do
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path (unlines (baselineHeader : map row results))
  where
    row r =
      componentName (mrComponent r)
        ++ "\t"
        ++ mrModule r
        ++ "\t"
        ++ fmt (mcExpressions (mrCounts r))
        ++ "\t"
        ++ fmt (mcBooleans (mrCounts r))
        ++ "\t"
        ++ fmt (mcAlternatives (mrCounts r))
    fmt (c, t) = show c ++ "\t" ++ show t

----------------------------------------------------------------------------
-- Baseline comparison
----------------------------------------------------------------------------

compareWithBaseline :: [BaselineRow] -> [ModuleResult] -> [String]
compareWithBaseline baseline current =
  drift
    ++ concatMap problemsFor current
    ++ zeroExecution
  where
    baseSet = sort [(brComponent b, brModule b) | b <- baseline]
    curSet = sort [(componentName (mrComponent c), mrModule c) | c <- current]
    drift =
      [ "module set drift: missing from current " ++ show m
      | m <- baseSet \\ curSet
      ]
        ++ [ "module set drift: not in baseline " ++ show m
           | m <- curSet \\ baseSet
           ]
    baseMap =
      Map.fromList [(brComponent b ++ "\t" ++ brModule b, brCounts b) | b <- baseline]
    problemsFor r =
      let key = componentName (mrComponent r) ++ "\t" ++ mrModule r
      in case Map.lookup key baseMap of
           Nothing -> []
           Just base ->
             metricProblem "expressions" (mcExpressions base) (mcExpressions (mrCounts r))
               ++ metricProblem "booleans" (mcBooleans base) (mcBooleans (mrCounts r))
               ++ metricProblem "alternatives" (mcAlternatives base) (mcAlternatives (mrCounts r))
    zeroExecution =
      [ "module with expression boxes executed no expressions: " ++ mrModule r
      | r <- current
      , let (cov, tot) = mcExpressions (mrCounts r)
      , tot > 0
      , cov == 0
      ]
    metricProblem name (bc, bt) (cc, ct)
      | bt == 0 && ct == 0 = [] -- N/A on both sides
      | bt == 0 = [name ++ " denominator is nonzero, but the baseline denominator is zero"]
      | ct == 0 = [name ++ " denominator is zero, but the baseline denominator is nonzero"]
      | bc * ct > cc * bt =
          [ "decreased "
              ++ name
              ++ ": baseline "
              ++ show (bc, bt)
              ++ " current "
              ++ show (cc, ct)
          ]
      | otherwise = []

(\\) :: Eq a => [a] -> [a] -> [a]
a \\ b = [x | x <- a, x `notElem` b]

----------------------------------------------------------------------------
-- Reports
----------------------------------------------------------------------------

-- | Write a tix file that contains only library data.
-- Run @hpc report@ and @hpc markup@ with this file.
-- Copy each module's .mix file into one directory without changing its contents.
-- Use @--hpcdir@ to select this directory instead of @.hpc@.
-- Save and print the report. Stop if the report is empty.
writeReports :: FilePath -> [ModuleResult] -> IO ()
writeReports dir results = do
  createDirectoryIfMissing True dir
  let mixDir = dir </> "mix"
      tixPath = dir </> "reflex-vty-library.tix"
  createDirectoryIfMissing True mixDir
  forM_ results $ \r -> do
    present <- doesFileExist (mrMixPath r)
    when present $
      copyFile (mrMixPath r) (mixDir </> mrModule r <.> "mix")
  writeTix tixPath (Tix results')
  reportOut <- try (readProcess "hpc" (["report", "--hpcdir=" ++ mixDir, tixPath]) "")
  case reportOut of
    Left (_ :: SomeException) -> die "hpc report failed"
    Right text
      | all isSpace text -> die "hpc report produced an empty report"
      | otherwise -> do
          writeFile (dir </> "report.txt") text
          putStr text
  code <- rawSystem "hpc" ["markup", "--hpcdir=" ++ mixDir, tixPath, "--destdir=" ++ (dir </> "markup")]
  unless (code == ExitSuccess) $ die "hpc markup failed"
  where
    results' =
      [ TixModule (mrModule r) (mrHash r) (length (mrTicks r)) (mrTicks r)
      | r <- results
      ]

----------------------------------------------------------------------------
-- Self-test
----------------------------------------------------------------------------

selfTestLabels :: [BoxLabel]
selfTestLabels =
  [ ExpBox True
  , ExpBox True
  , ExpBox False
  , BinBox GuardBinBox True
  , BinBox GuardBinBox False
  , BinBox CondBinBox True
  , BinBox CondBinBox False
  , TopLevelBox ["alt"]
  , LocalBox ["case"]
  ]

selfTestPositions :: [HpcPos]
selfTestPositions = [toHpcPos (i, 1, i, 2) | i <- [1 :: Int ..]]

-- | A fixed past time for modification-time comparisons.
pastTime :: UTCTime
pastTime = read "2000-01-01 00:00:00 UTC"

selfTest :: IO ()
selfTest = withTempDir $ \dir -> do
  let mixDir = dir </> "mix"
      tixDir = dir </> "tix"
  createDirectoryIfMissing True mixDir
  createDirectoryIfMissing True tixDir
  let mkMix name hash =
        mixCreate
          mixDir
          name
          ( Mix
              ("src" </> name)
              pastTime
              hash
              (length selfTestLabels)
              (zip selfTestPositions selfTestLabels)
          )
  mkMix "Mod.Executed" 11
  mkMix "Mod.NeverRun" 12
  mkMix "Test.Helper" 13
  mkMix "Mod.Internal" 14
  let executedTicks = [1, 1, 0, 1, 1, 0, 1, 0, 1] :: [Integer]
  writeTix
    (tixDir </> "suite.tix")
    ( Tix
        [ TixModule "mix/Mod.Executed" 11 (length executedTicks) executedTicks
        , TixModule "dependency/Mod.Executed" 99 (length executedTicks) executedTicks
        , TixModule "Test.Helper" 13 (length executedTicks) executedTicks
        ]
    )
  let mods =
        [ (CompMain, "Mod.Executed")
        , (CompMain, "Mod.NeverRun")
        , (CompInternal, "Mod.Internal")
        ]
      mixes =
        [ mixDir </> "Mod.Executed" <.> "mix"
        , mixDir </> "Mod.NeverRun" <.> "mix"
        , mixDir </> "Mod.Internal" <.> "mix"
        ]
  results <- analyze dir mods mixes [tixDir </> "suite.tix"] []
  -- 1. Count the boxes of an unexecuted module in the denominator.
  case [r | r <- results, mrModule r == "Mod.NeverRun"] of
    (r : _) -> do
      expectEq (mcExpressions (mrCounts r)) (0, 3) "omitted module contributes expressions denominator"
      expectEq (mcBooleans (mrCounts r)) (0, 2) "omitted module contributes boolean denominator"
    [] -> die "self-test: missing omitted module"
  -- 2. Count internal modules in the denominator.
  case [r | r <- results, mrComponent r == CompInternal] of
    (r : _) -> do
      expectEq (mrTicks r) (replicate (length executedTicks) 0) "internal module without tix has zero ticks"
      expectEq (mcExpressions (mrCounts r)) (0, 3) "internal module denominator"
    [] -> die "self-test: missing internal module"
  -- 3. Exclude test modules and dependency modules.
  expectEq (length results) 3 "test/dependency modules excluded"
  -- 4. Pair the True and False boxes from the same condition.
  case [r | r <- results, mrModule r == "Mod.Executed"] of
    (r : _) -> do
      expectEq (mcExpressions (mrCounts r)) (2, 3) "expression classification"
      expectEq (mcBooleans (mrCounts r)) (1, 2) "boolean pair classification"
      expectEq (mcAlternatives (mrCounts r)) (2, 2) "alternative classification"
    [] -> die "self-test: missing executed module"
  -- 5. Add child tick counts without replacing parent counts.
  writeTix
    (tixDir </> "child-a.tix")
    (Tix [TixModule "mix/Mod.Executed" 11 (length executedTicks) (map (* 2) executedTicks)])
  merged <-
    analyze
      dir
      [(CompMain, "Mod.Executed")]
      [mixDir </> "Mod.Executed" <.> "mix"]
      [tixDir </> "suite.tix"]
      [tixDir </> "child-a.tix"]
  case merged of
    (r : _) ->
      expectEq
        (mrTicks r)
        (zipWith (+) executedTicks (map (* 2) executedTicks))
        "child ticks merge additively"
    [] -> die "self-test: merge produced nothing"
  -- 6. Reject a hash mismatch.
  writeTix
    (tixDir </> "badhash.tix")
    (Tix [TixModule "Mod.Executed" 99 (length executedTicks) executedTicks])
  hashFailure <-
    dies
      ( analyze
          dir
          [(CompMain, "Mod.Executed")]
          [mixDir </> "Mod.Executed" <.> "mix"]
          [tixDir </> "badhash.tix"]
          []
      )
  expectEq hashFailure True "hash mismatch fails"
  -- 7. Reject a tick array if its length differs from the declared length.
  writeTix
    (tixDir </> "badlen.tix")
    (Tix [TixModule "Mod.Executed" 11 (length executedTicks) (take 7 executedTicks)])
  lenFailure <-
    dies
      ( analyze
          dir
          [(CompMain, "Mod.Executed")]
          [mixDir </> "Mod.Executed" <.> "mix"]
          [tixDir </> "badlen.tix"]
          []
      )
  expectEq lenFailure True "declared tick length mismatch fails"
  -- 8. Accept child files only between the marker time and the parent tix time, inclusive.
  let marker = pastTime
      parent = addUTCTime 100 marker
      justBefore = addUTCTime (-1) marker
      justAfter = addUTCTime 101 parent
      ok = addUTCTime 50 marker
  expectEq (childTimeProblem marker parent "c.tix" ok) Nothing "fresh child tix accepted"
  expectEq
    (fmap (containsInfix "run-start marker") (childTimeProblem marker parent "c.tix" justBefore))
    (Just True)
    "child tix predating the run-start marker fails"
  expectEq
    (fmap (containsInfix "newer than parent") (childTimeProblem marker parent "c.tix" justAfter))
    (Just True)
    "child tix newer than the parent tix fails"
  expectEq (knownSignalTixName "signal-sigint.tix") True "signal child name accepted"
  expectEq (knownSignalTixName "signal-sigterm.tix") True "second signal child name accepted"
  expectEq (knownSignalTixName "stale-sigint.tix") False "non-signal child name rejected"
  expectEq (knownSignalTixName "signal-.tix") False "empty signal mode rejected"
  expectEq (knownSignalTixName "signal-x/y.tix") False "path separators rejected"
  -- 9. Compare directory components, not text prefixes.
  expectEq (isUnderDir "/x/a.tix" "/x") True "isUnderDir direct child"
  expectEq (isUnderDir "/x/y/a.tix" "/x") True "isUnderDir deep child"
  expectEq (isUnderDir "/xy/a.tix" "/x") False "isUnderDir prefix is not containment"
  expectEq (isUnderDir (normalise "/x/child/../a.tix") "/x") True "isUnderDir after normalisation"
  -- 10. Fail if a baseline module is missing.
  let baselineRows =
        [ BaselineRow "main" "Mod.Executed" (ModuleCounts (2, 3) (1, 2) (2, 2))
        , BaselineRow "main" "Mod.Gone" (ModuleCounts (1, 1) (0, 0) (0, 0))
        ]
  problems <- pure (compareWithBaseline baselineRows results)
  expectEq (any (containsInfix "module set drift") problems) True "missing baseline module fails"
  -- 11. Fail if the coverage ratio decreases, even if the covered count increases.
  let lowerRatio =
        compareWithBaseline
          [BaselineRow "main" "M" (ModuleCounts (1, 4) (0, 0) (0, 0))]
          [ModuleResult CompMain "M" (ModuleCounts (2, 20) (0, 0) (0, 0)) 0 [] ""]
  expectEq (any (containsInfix "decreased expressions") lowerRatio) True "lower ratio with higher hits fails"
  -- 12. Treat zero denominators on both sides as N/A.
  expectEq
    ( compareWithBaseline
        [BaselineRow "main" "N" (ModuleCounts (0, 0) (0, 0) (0, 0))]
        [ModuleResult CompMain "N" (ModuleCounts (0, 0) (0, 0) (0, 0)) 0 [] ""]
    )
    []
    "N/A handling"
  putStrLn "self-test: OK"
  where
    expectEq actual expected label =
      when (actual /= expected) $
        die ("self-test failure (" ++ label ++ "): expected " ++ show expected ++ ", got " ++ show actual)
    containsInfix needle hay = any (needle `prefixOf`) (tails' hay)
    prefixOf p s = take (length p) s == p
    tails' s = case s of
      [] -> [[]]
      (_ : rest) -> s : tails' rest

-- | Return 'True' if the action raises an exception.
dies :: IO a -> IO Bool
dies act = do
  r <- try act
  pure $ case r of
    Left (_ :: SomeException) -> True
    Right _ -> False
