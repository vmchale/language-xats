module Language.XATS.Type.Syntax.Static ( StaExp (..)
                                        ) where

-- | Defined
-- [here](https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/SATS/staexp0.sats#L741)
data StaExp a = StaInt a Int
              | StaChr a Char
              | StaFloat a Float
              | StaStr a BSL.ByteString
