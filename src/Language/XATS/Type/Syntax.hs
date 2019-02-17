-- | This is a module containing a type for the syntax as it is intially
-- parsed; this package mirrors the approach of the ATS2 code upstream and thus
-- we may have tokens etc. contained in the syntax node; these will be
-- transformed later.
module Language.XATS.Type.Syntax ( StaDeclSyn (..)
                                 , DynDeclSyn (..)
                                 , StaDecls
                                 , DynDecls
                                 ) where

import qualified Data.ByteString.Lazy as BSL

type StaDecls a = [StaDeclSyn a]

type DynDecls a = [DynDeclSyn a]

data StaDeclSyn a

data DynDeclSyn a
