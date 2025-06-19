module WithRawInput ( withRawInput ) where

#ifdef mingw32_HOST_OS
{- Windows implementation -}
import WithRawInputWin32 (withRawInput)
#else
{- POSIX implementation -}
{- from unix library -}
import System.Posix.Terminal
import System.Posix.IO (stdInput)

{- from base -}
import Control.Exception (finally)
#endif

#ifndef mingw32_HOST_OS
{- https://stackoverflow.com/questions/23068218/haskell-read-raw-keyboard-input
 - run an application in raw input / non-canonical mode with given
 - VMIN and VTIME settings. for a description of these, see:
 - http://www.gnu.org/software/libc/manual/html_node/Noncanonical-Input.html
 - as well as `man termios`.
 -}
withRawInput :: Int -> Int -> IO a -> IO a
withRawInput vmin vtime application = do

  {- retrieve current settings -}
  oldTermSettings <- getTerminalAttributes stdInput

  {- modify settings -}
  let newTermSettings =
        flip withoutMode  EnableEcho   . -- don't echo keystrokes
        flip withoutMode  ProcessInput . -- turn on non-canonical mode
        flip withTime     vtime        . -- wait at most vtime decisecs per read
        flip withMinInput vmin         $ -- wait for >= vmin bytes per read
        oldTermSettings

  {- install new settings -}
  setTerminalAttributes stdInput newTermSettings Immediately

  {- restore old settings no matter what; this prevents the terminal
   - from becoming borked if the application halts with an exception
   -}
  application
    `finally` setTerminalAttributes stdInput oldTermSettings Immediately
#endif


