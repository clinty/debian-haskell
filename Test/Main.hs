module Main where

import Test.HUnit
import System.Exit
import Changes
import Control
import Versions
import SourcesList
import Dependencies
import Text.PrettyPrint.ANSI.Leijen (Doc, text, (<+>), (<$>), fillSep, renderPretty, displayS)

main =
    do (c,st) <- runTestText putTextToShowS (TestList (versionTests ++ sourcesListTests ++ dependencyTests ++ changesTests ++ controlTests ++ prettyTests))
       putStrLn (st "")
       case (failures c) + (errors c) of
         0 -> return ()
         _ -> exitFailure

-- | I was converting from one pretty printing package to another and
-- was unclear how this should work.
prettyTests =
    [ TestCase (assertEqual
                "pretty0"
                (unlines
                 ["Usage: debian-report <old sources.list> <new sources.list>",
                  "",
                  "Find all the packages referenced by the",
                  "second sources.list which trump packages",
                  "find in the first sources.list."])
                (displayS (renderPretty 1.0 40 (helpText "debian-report")) "")
               ) ]

helpText :: String -> Doc
helpText progName =
    (text "Usage:" <+> text progName <+> text "<old sources.list>" <+> text "<new sources.list>" <$>
     text [] <$>
     (fillSep $ map text $ words $ "Find all the packages referenced by the second sources.list which trump packages find in the first sources.list.") <$>
     text []
    )
