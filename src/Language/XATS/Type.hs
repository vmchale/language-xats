{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Language.XATS.Type ( Declaration (..)
                          , XATS (..)
                          , FixityNode (..)
                          , Fixity (..)
                          , FixityRes (..)
                          , SymEnv
                          ) where

import           Control.DeepSeq      (NFData)
import qualified Data.ByteString.Lazy as BSL
import           Data.List.NonEmpty   (NonEmpty (..))
import           GHC.Generics         (Generic)
import           GHC.Word             (Word8)
import           Language.XATS.Type.SymEnv

newtype XATS a = XATS [Declaration a]
    deriving (Eq, Generic, NFData)

-- | Allows both @#prefix 99 !@ and @#infixl ( && ) andalso land@
data FixityRes a = IntFix a Word8
                 | SymbolFix a BSL.ByteString
                 deriving (Eq, Generic, NFData)
               -- TODO: parens? https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/TEST/DATA/syntax_sta.sats#L111

data Fixity a = Fixity (FixityRes a) (NonEmpty (FixityNode a))
              deriving (Eq, Generic, NFData)

data Declaration a = PrefixDecl a (Fixity a)
                   | Infix0Decl a (Fixity a)
                   | InfixrDecl a (Fixity a)
                   | InfixDecl a (Fixity a)
                   | InfixlDecl a (Fixity a)
                   | PostfixDecl a (Fixity a)
                   deriving (Eq, Generic, NFData)
