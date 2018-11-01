{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- | See
-- [here](https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/SATS/staexp0.sats#L300)
-- for the upstream version
module Language.XATS.Type ( Declaration (..)
                          , XATS (..)
                          , FixityNode (..)
                          , FixityRes (..)
                          , FixityD (..)
                          , Expression (..)
                          , StaticExpression
                          , StaEffect (..)
                          , StaDeclaration (..)
                          -- * Reëxports
                          , SymEnv
                          , Token (..)
                          , Special (..)
                          , Keyword (..)
                          , Addendum (..)
                          , LambdaAdd (..)
                          , FunFlavor (..)
                          , ImplFlavor (..)
                          ) where

import           Control.DeepSeq           (NFData)
import qualified Data.ByteString.Lazy      as BSL
import           Data.List.NonEmpty        (NonEmpty (..))
import qualified Data.Text                 as T
import           GHC.Generics              (Generic)
import           GHC.Word                  (Word8)
import           Language.XATS.Type.Lexer
import           Language.XATS.Type.SymEnv

newtype XATS a = XATS [Declaration a]
    deriving (Eq, Generic, NFData)

-- | Allows both @#prefix 99 !@ and @#infixl ( && ) andalso land@
data FixityRes a = IntFix a Word8
                 | SymbolFix a BSL.ByteString
                 -- TODO: handle https://github.com/githwxi/ATS-Xanadu/blob/master/prelude/fixity.sats#L136
                 deriving (Eq, Generic, NFData)
               -- TODO: parens? https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/TEST/DATA/syntax_sta.sats#L111

data FixityD a = FixityD (FixityRes a) (NonEmpty (FixityNode a))
              deriving (Eq, Generic, NFData)

data StaEffect a = NoneEff -- ^ @:@
                 | SomeEff [StaticExpression a] -- ^ @:<eff0,eff1>@

-- | See
-- [here](https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/SATS/staexp0.sats#L742)
data StaticExpression a

-- TODO: separate dynamic/static expressions
data Expression a = IntLit a Integer
                  | DoubleLit a Double

data Declaration a = StaDeclaration a (StaDeclaration a)
    deriving (Eq, Generic, NFData)

-- FIXME: generalize these?
data StaDeclaration a = PrefixDecl { staDeclLoc :: a, fixBody :: FixityD a }
                      | Infix0Decl { staDeclLoc :: a, fixBody :: FixityD a }
                      | InfixrDecl { staDeclLoc :: a, fixBody :: FixityD a }
                      | InfixDecl { staDeclLoc :: a, fixBody :: FixityD a }
                      | InfixlDecl { staDeclLoc :: a, fixBody :: FixityD a }
                      | PostfixDecl { staDeclLoc :: a, fixBody :: FixityD a }
                      | AbsSort { staDeclLoc :: a, absSortName :: T.Text }
                      deriving (Eq, Generic, NFData)
