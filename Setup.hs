#!/usr/bin/runhaskell

import Distribution.Simple
import Distribution.Simple.Program
import System.Directory
import System.Exit
import System.Process

main = copyFile "debian/changelog" "changelog" >>
       defaultMainWithHooks simpleUserHooks {
         postBuild = \ _ _ _ _ -> runTestScript
       , runTests = \ _ _ _ _ -> runTestScript
       }

runTestScript =
    system "runhaskell Test/Main.hs" >>= \ code ->
    if code == ExitSuccess then return () else error "Test Failure"
