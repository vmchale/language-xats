module Language.XATS.Type.Syntax.Static ( StaExp (..)
                                        ) where

import qualified Data.ByteString.Lazy      as BSL
import           Language.XATS.Type.Common
import           Language.XATS.Type.Lexer

-- | Defined
-- [here](https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/SATS/staexp0.sats#L741)
data StaExp a = StaIdent a (Ident a)
              | StaOp1 a (Token a)
              | StaOp2 a (Token a) (Ident a) (Token a)
              | StaInt a Int
              | StaChr a Char
              | StaFloat a Float
              | StaStr a BSL.ByteString
