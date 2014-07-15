{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Debian.Deb where

import Control.Monad

import Debian.Control.Common
import System.Directory (canonicalizePath, getTemporaryDirectory)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import System.Unix.Directory (withTemporaryDirectory, withWorkingDirectory)

fields :: (ControlFunctions a) => FilePath -> IO (Control' a)
fields debFP =
    withTemporaryDirectory ("fields.XXXXXX") $ \tmpdir ->
      do debFP <- canonicalizePath debFP
         withWorkingDirectory tmpdir $
           do (res, out, err) <- readProcessWithExitCode "ar" ["x",debFP,"control.tar.gz"] ""
              when (res /= ExitSuccess) (error $ "Dpkg.fields: " ++ show out ++ "\n" ++ show err ++ "\n" ++ show res)
              (res, out, err) <- readProcessWithExitCode "tar" ["xzf", "control.tar.gz", "./control"] ""
              when (res /= ExitSuccess) (error $ "Dpkg.fields: " ++ show out ++ "\n" ++ show err ++ "\n" ++ show res)
              c <- parseControlFromFile "control"
              case c of
                Left e -> error (show e)
                (Right c) -> return c -- I don't think we need seq because parsec will force everything from the file
