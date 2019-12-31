{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Reflex.Network
import Reflex.Process
import Reflex.Vty

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Graphics.Vty as V
import qualified System.Process as P
import System.IO.Temp (withSystemTempDirectory)

main :: IO ()
main = withSystemTempDirectory "asdf" $ \fp -> mainWidget $ do
  exit <- keyCombo (V.KChar 'c', [V.MCtrl])
  t <- tickLossyFromPostBuildTime 1
  col $ do
    -- Repeatedly call the `date` command
    out <- fixed 2 $ fmap (switch . current) $ networkHold (return never) $ ffor t $ \t -> do
      (Process { _process_stdout = out }) <- createProcess (P.proc "date" []) never
      row $ do
        fixed 10 $ text $ pure "Tick:"
        stretch $ display $ pure t
      return out
    -- Display the stdout output of the date command
    fixed 2 $ do
      row $ do
        fixed 10 $ text "stdout:"
        stretch $ text <=< hold "" $ T.decodeUtf8 <$> out
    -- Run an interactive command and get user input
    stretch $ col $ do
      fixed 2 $ text "Running interactive command. You'll be prompted to remove an empty file that this program has created."
      let tmpfile = fp <> "/my-temporary-file"
      (Process { _process_exit = touchExit }) <- fixed 1 $ do
        text $ pure $ "$> touch " <> T.pack tmpfile
        createProcess (P.proc "touch" [tmpfile]) never
      fixed 1 $ text <=< hold "" $ "File created." <$ touchExit
      fixed 1 $ text $ pure $ "$>  rm -i " <> T.pack tmpfile
      fixed 4 $ do
        rec pout <- createProcess (P.proc "rm" ["-i", fp <> "/my-temporary-file"]) $ fmap T.encodeUtf8 $
              tag (current (_textInput_value i)) enter
            (i, enter) <- col $ do
              fixed 1 $ text <=< hold "" $ T.decodeUtf8 <$> _process_stderr pout
              fixed 3 $ do
                i <- boxStatic def $ textInput def
                enter <- key V.KEnter
                return (i, enter)
        display <=< hold Nothing $ Just <$> _process_exit pout
        return ()
  return $ () <$ exit
