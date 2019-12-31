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
    fixed 6 $ boxStatic def $ col $ do
      fixed 1 $ text "Call the date command every second and display the result."
      out <- fixed 2 $ fmap (switch . current) $ networkHold (return never) $ ffor t $ \t -> do
        (Process { _process_stdout = out }) <- createProcess (P.proc "date" []) never
        row $ do
          fixed 10 $ text $ pure "Tick:"
          stretch $ display $ pure t
        return out
      fixed 2 $ do
        row $ do
          fixed 10 $ text "stdout:"
          stretch $ text <=< hold "" $ T.decodeUtf8 <$> out

    stretch $ boxStatic def $ col $ do
      fixed 3 $ text "Running interactive command. You'll be prompted to remove an empty file that this program has created."
      let tmpfile = fp <> "/my-temporary-file"
      (Process { _process_exit = touchExit }) <- fixed 1 $ do
        text $ pure $ "$> touch " <> T.pack tmpfile
        createProcess (P.proc "touch" [tmpfile]) never
      fixed 1 $ text <=< hold "" $ "File created." <$ touchExit
      fixed 1 $ text $ pure $ "$> rm -i " <> T.pack tmpfile
      rec pout <- fixed 3 $ do
            pout <- createProcess (P.proc "rm" ["-i", fp <> "/my-temporary-file"]) userInput
            col $ do
              _ <- fixed 1 $ prefix "stdout:" $
                text <=< hold "<no stdout yet>" $ T.decodeUtf8 <$> _process_stdout pout
              _ <- fixed 1 $ prefix "stderr:" $
                text <=< hold "<no stderr yet>" $ T.decodeUtf8 <$> _process_stderr pout
              _ <- fixed 1 $ prefix "exit code:" $
                display =<< hold Nothing (Just <$> _process_exit pout)
              return pout
          (i, enter) <- fixed 3 $ do
            i <- boxStatic def $ prefix "Your response:" $ textInput def
            enter <- key V.KEnter
            return (i, enter)
          let userInput = fmap T.encodeUtf8 $ tag (current (_textInput_value i)) enter
      fixed 1 $ prefix "exit code:" $
        display <=< hold Nothing $ Just <$> _process_exit pout
      fixed 1 $ prefix "enter pressed:" $
        display <=< hold Nothing $ Just <$> enter
      fixed 1 $ prefix "user input:" $
        display <=< hold Nothing $ Just <$> userInput
  return $ () <$ exit
  where
    prefix t a = row $ do
      fixed (pure $ T.length t + 1) $ text $ pure t
      stretch a
