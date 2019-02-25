-- | This is a module containing a type for the syntax as it is intially
-- parsed; this package mirrors the approach of the ATS2 code upstream and thus
-- we may have tokens etc. contained in the syntax node; these will be
-- transformed later.
module Language.XATS.Type.Syntax ( DynDeclSyn (..)
                                 , DynDecls
                                 , PrecOpt (..)
                                 , PrecMod (..)
                                 , SignInt (..)
                                 ) where

import           Language.XATS.Type.Common
import           Language.XATS.Type.Lexer

type DynDecls a = [DynDeclSyn a]

-- | Defined
-- [here](https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/SATS/dynexp0.sats#L1065)
data PrecOpt a = PrecOptNil a
               | PrecOptInt (Token a)
               | PrecOptOpt (Ident a) (PrecMod a)

-- | Defined
-- [here](https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/SATS/dynexp0.sats#L1071)
data PrecMod a = PrecModNone a
               | PrecModSome a (Token a) (SignInt a) (Token a)

-- | Defined
-- [here](https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/SATS/dynexp0.sats#L1076)
data SignInt a = SignIntInt a (Token a)
               | SignIntOpr a (Token a) (Token a)

-- | See
-- [here](https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/SATS/dynexp0.sats#L954)
data DynDeclSyn a = Dnonfix a (Token a) [Ident a] -- TODO: NonEmpty?
                  | Dfixity a (Token a) (PrecOpt a)
