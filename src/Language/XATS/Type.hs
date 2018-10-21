module Language.XATS.Type ( Declaration (..)
                          , XATS (..)
                          , FixityNode (..)
                          , Fixity (..)
                          , FixityRes (..)
                          ) where

import qualified Data.ByteString.Lazy as BSL
import           Data.List.NonEmpty   (NonEmpty (..))

newtype XATS a = XATS [Declaration a]

data FixityNode a = Symbol a BSL.ByteString
                  | FixityIdent a BSL.ByteString

-- | Allows both @#prefix 99 !@ and @#infixl ( && ) andalso land@
data FixityRes a = IntFix a Word -- if you use fixities that overflow, you deserve bugs
                 | SymbolFix a BSL.ByteString
               -- TODO: parens? https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/TEST/DATA/syntax_sta.sats#L111

data Fixity a = PrefixDecl { locFix :: a, assigned :: FixityRes a, asignees :: NonEmpty (FixityNode a) }

data Declaration a = FixityDecl a (Fixity a)
