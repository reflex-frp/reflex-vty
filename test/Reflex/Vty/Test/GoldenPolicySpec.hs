module Reflex.Vty.Test.GoldenPolicySpec (spec) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , getPermissions
  , setPermissions
  , writable
  )
import System.FilePath ((<.>), (</>))
import System.IO (IOMode (ReadWriteMode), withBinaryFile)
import System.IO.Extra (withTempDir)
import Test.Hspec

import Reflex.Vty.Test.Snapshot

spec :: Spec
spec = describe "Golden policy" $ do
  describe "CheckGoldens" $ do
    it "fails on a missing file and creates nothing" $
      withTempDir $ \dir -> do
        let goldenDir = dir </> "goldens"
            name = "missing_fixture"
        result <-
          try (assertGolden CheckGoldens goldenDir name "actual") :: IO (Either SomeException ())
        case result of
          Left _ -> pure ()
          Right () -> expectationFailure "expected a failure for a missing golden file"
        doesFileExist (goldenDir </> name <.> "txt") `shouldReturn` False
        doesDirectoryExist goldenDir `shouldReturn` False

    it "passes when the file matches exactly" $
      withTempDir $ \dir -> do
        let name = "matching"
            path = dir </> name <.> "txt"
        BS.writeFile path (TE.encodeUtf8 (T.pack "expected\n"))
        assertGolden CheckGoldens dir name "expected\n"

    it "fails on a mismatch, leaving the file unchanged" $
      withTempDir $ \dir -> do
        let name = "mismatched"
            path = dir </> name <.> "txt"
        BS.writeFile path (TE.encodeUtf8 (T.pack "expected\n"))
        result <-
          try (assertGolden CheckGoldens dir name "actual\n") :: IO (Either SomeException ())
        case result of
          Left _ -> pure ()
          Right () -> expectationFailure "expected a failure for a mismatched golden file"
        BS.readFile path `shouldReturn` TE.encodeUtf8 (T.pack "expected\n")

    it "fails when a golden file is locked for writing" $
      withTempDir $ \dir -> do
        let name = "locked"
            path = dir </> name <.> "txt"
        BS.writeFile path (TE.encodeUtf8 (T.pack "expected\n"))
        result <- withBinaryFile path ReadWriteMode $ \_ ->
          try (assertGolden CheckGoldens dir name "expected\n") :: IO (Either SomeException ())
        case result of
          Left _ -> pure ()
          Right () -> expectationFailure "expected a failure for a locked golden file"

  describe "UpdateGoldens" $ do
    it "creates a missing file with the exact UTF-8 bytes, including the directory" $
      withTempDir $ \dir -> do
        let goldenDir = dir </> "goldens"
            name = "created"
        assertGolden UpdateGoldens goldenDir name "a\30028b\n"
        BS.readFile (goldenDir </> name <.> "txt")
          `shouldReturn` TE.encodeUtf8 (T.pack "a\30028b\n")

    it "overwrites a changed file" $
      withTempDir $ \dir -> do
        let name = "stale"
            path = dir </> name <.> "txt"
        BS.writeFile path (TE.encodeUtf8 (T.pack "old\n"))
        assertGolden UpdateGoldens dir name "new\n"
        BS.readFile path `shouldReturn` TE.encodeUtf8 (T.pack "new\n")

    it "leaves an equal file untouched" $
      withTempDir $ \dir -> do
        let name = "equal"
            path = dir </> name <.> "txt"
        BS.writeFile path (TE.encodeUtf8 (T.pack "same\n"))
        perms <- getPermissions path
        setPermissions path perms {writable = False}
        assertGolden UpdateGoldens dir name "same\n"
        -- The file was not rewritten. An attempted write would have
        -- failed against the write-protected file.
        BS.readFile path `shouldReturn` TE.encodeUtf8 (T.pack "same\n")
