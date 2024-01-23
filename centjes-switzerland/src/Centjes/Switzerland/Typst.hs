module Centjes.Switzerland.Typst
  ( compileTypst,
  )
where

import Path
import System.Process.Typed

compileTypst :: Path Abs File -> Path Abs File -> IO ()
compileTypst inputFile outputFile = do
  runProcess_ $
    setWorkingDir (fromAbsDir (parent inputFile)) $
      setStdout inherit $
        setStderr inherit $
          proc
            "typst"
            [ "-v",
              "compile",
              fromAbsFile inputFile,
              fromAbsFile outputFile
            ]
