module WithRawInputWin32 ( withRawInput ) where

{- Windows implementation of withRawInput
 - This is a simplified implementation that just runs the application
 - without changing console modes. This allows the project to compile
 - on Windows, though raw input functionality is not implemented.
 - The vmin and vtime parameters are ignored as they're POSIX-specific.
 -}
withRawInput :: Int -> Int -> IO a -> IO a
withRawInput _vmin _vtime application = application