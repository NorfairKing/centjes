module Centjes.Logging.TestUtils
  ( evaluatingLogFunc,
    runTestLoggingT,
  )
where

import Control.Exception
import Control.Monad.Logger

runTestLoggingT :: LoggingT m a -> m a
runTestLoggingT = flip runLoggingT evaluatingLogFunc

evaluatingLogFunc :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
evaluatingLogFunc loc source level str = do
  _ <- evaluate loc
  _ <- evaluate source
  _ <- evaluate level
  _ <- evaluate str
  pure ()
