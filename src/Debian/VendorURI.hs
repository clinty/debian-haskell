{-# LANGUAGE TemplateHaskell #-}

module Debian.VendorURI
    ( VendorURI(..)
    , vendorURI
    , parseVendorURI
    ) where

import Control.Lens (makeLenses, review)
import Debian.URI (parseURI, URI())
import Language.Haskell.TH.Syntax (Loc)

newtype VendorURI = VendorURI {_vendorURI :: URI} deriving (Eq, Ord)

instance Show VendorURI where
    show (VendorURI uri) = "VendorURI (fromJust (parseURIReference " ++ show (show uri) ++ "))"

$(makeLenses ''VendorURI)

parseVendorURI :: [Loc] -> String -> Maybe VendorURI
parseVendorURI locs s = fmap (review vendorURI) (parseURI s)

-- toURI' :: VendorURI -> URI'
-- toURI' = URI' . show . view vendorURI
