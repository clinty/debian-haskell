{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Debian.Control.PrettyPrint where

import Data.List (intersperse)
import qualified Data.ByteString.Char8 as C
import Text.PrettyPrint.ANSI.Leijen

import Debian.Control.Common

ppControl :: (ToText a) => Control' a -> Doc
ppControl (Control paragraph) =
    vcat (map ppParagraph paragraph)

ppParagraph :: (ToText a) => Paragraph' a -> Doc
ppParagraph (Paragraph fields) =
    vcat (map ppField fields ++ [empty])

ppField :: (ToText a) => Field' a -> Doc
ppField (Field (n,v)) = totext n <> text ":" <> totext v


class ToText a where
    totext :: a -> Doc

instance ToText String where
    totext = text

instance ToText C.ByteString where
    totext = text . C.unpack
