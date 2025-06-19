module WithRawInputWin32 ( withRawInput ) where

{- from Win32 library -}
import System.Win32.Console
import System.Win32.Types

{- from base -}
import Control.Exception (bracket)

{- Windows implementation of withRawInput
 - This function sets up raw input mode on Windows console
 - vmin and vtime parameters are ignored on Windows as they're POSIX-specific
 - We use the Windows console API to disable line input and echo
 -}
withRawInput :: Int -> Int -> IO a -> IO a
withRawInput _vmin _vtime application = do
  hStdin <- getStdHandle sTD_INPUT_HANDLE
  
  bracket
    (do
      -- Get current console mode
      oldMode <- getConsoleMode hStdin
      -- Disable line input and echo input
      let newMode = oldMode .&. complement (eNABLE_LINE_INPUT .|. eNABLE_ECHO_INPUT)
      setConsoleMode hStdin newMode
      return oldMode)
    (\oldMode -> do
      -- Restore original console mode
      setConsoleMode hStdin oldMode)
    (\_ -> application)