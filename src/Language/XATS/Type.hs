{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Language.XATS.Type ( Declaration (..)
                          , XATS (..)
                          , FixityNode (..)
                          , FixityRes (..)
                          , FixityD (..)
                          , Expression (..)
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
import           GHC.Generics              (Generic)
import           GHC.Word                  (Word8)
import           Language.XATS.Type.Lexer
import           Language.XATS.Type.SymEnv

newtype XATS a = XATS [Declaration a]
    deriving (Eq, Generic, NFData)

-- | Allows both @#prefix 99 !@ and @#infixl ( && ) andalso land@
data FixityRes a = IntFix a Word8
                 | SymbolFix a BSL.ByteString
                 -- TODO: handle https://github.com/githwxi/ATS-Xanadu/blob/master/prelude/fixity.sats#L136<Paste>
                 deriving (Eq, Generic, NFData)
               -- TODO: parens? https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/TEST/DATA/syntax_sta.sats#L111

data FixityD a = FixityD (FixityRes a) (NonEmpty (FixityNode a))
              deriving (Eq, Generic, NFData)

-- TODO: separate dynamic/static expressions
data Expression a = IntLit a Integer
                  | DoubleLit a Double

-- FIXME: generalize these?
data Declaration a = PrefixDecl a (FixityD a)
                   | Infix0Decl a (FixityD a)
                   | InfixrDecl a (FixityD a)
                   | InfixDecl a (FixityD a)
                   | InfixlDecl a (FixityD a)
                   | PostfixDecl a (FixityD a)
                   deriving (Eq, Generic, NFData)
