module Language.XATS.Type ( Declaration (..)
                          , XATS (..)
                          , FixityNode (..)
                          , Fixity (..)
                          , FixityRes (..)
                          ) where

import qualified Data.ByteString.Lazy as BSL
import           Data.List.NonEmpty   (NonEmpty (..))
import           GHC.Word

newtype XATS a = XATS [Declaration a]

data FixityNode a = Symbol a BSL.ByteString
                  | FixityIdent a BSL.ByteString

-- | Allows both @#prefix 99 !@ and @#infixl ( && ) andalso land@
data FixityRes a = IntFix { locFixRes :: a, intFixRes :: Word } -- if you use fixities that overflow, you deserve bugs
                 | SymbolFix { locFixRes :: a, symFixRes :: BSL.ByteString }
               -- TODO: parens? https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/TEST/DATA/syntax_sta.sats#L111

data Fixity a = Fixity { locFix :: a, assigned :: FixityRes a, asignees :: NonEmpty (FixityNode a) }

data Declaration a = PrefixDecl a (Fixity a)
                   | Infix0Decl a (Fixity a)
                   | InfixrDecl a (Fixity a)
                   | InfixDecl a (Fixity a)
                   | InfixlDecl a (Fixity a)
                   | PostfixDecl a (Fixity a)
