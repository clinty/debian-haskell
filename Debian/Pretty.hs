-- | A constructor we can wrap around values to avoid any built in
-- Pretty instance - for example, instance Pretty [a].
--
--  * display is now prettyShow
--  * display' is now prettyText
--  * ppDisplay is now ppShow
--  * ppDisplay' is now ppText
{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, OverloadedStrings, TypeSynonymInstances #-}
module Debian.Pretty
    ( PP(PP, unPP)
    , prettyShow
    , prettyText
    , ppPrint
    , ppShow
    , ppText
    ) where

import Data.Text (Text, unpack, pack)
import Text.PrettyPrint.HughesPJClass (Doc, Pretty(pPrint), text, empty, prettyShow)

-- | This type is wrapped around values before we pretty print them so
-- we can write our own Pretty instances for common types without
-- polluting the name space of clients of this package with instances
-- they don't want.
newtype PP a = PP {unPP :: a} deriving (Functor)

instance Pretty (PP Text) where
    pPrint = text . unpack . unPP

instance Pretty (PP String) where
    pPrint = text . unPP

instance Pretty (PP a) => Pretty (PP (Maybe a)) where
    pPrint = maybe empty ppPrint . unPP

prettyText :: Pretty a => a -> Text
prettyText = pack . prettyShow

ppPrint :: Pretty (PP a) => a -> Doc
ppPrint = pPrint . PP

ppShow :: Pretty (PP a) => a -> String
ppShow = prettyShow . PP

ppText :: Pretty (PP a) => a -> Text
ppText = pack . prettyShow . PP
