{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.XATS.Type.SymEnv ( FixityNode (..)
                                 , SymEnv
                                 , Fixity (..)
                                 -- * Default env
                                 , defaultFixity
                                 ) where

import           Control.DeepSeq      (NFData)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map             as M
import           GHC.Generics         (Generic)
import           GHC.Word             (Word8)

data Fixity = Infixl Word8
            | Infixr Word8
            | Infix0 Word8
            | Nonfix
            | Prefix Word8
            | Postfix

type SymEnv = M.Map (FixityNode ()) Fixity

data FixityNode a = Symbol a BSL.ByteString
                  | FixityIdent a BSL.ByteString
                  deriving (Eq, Ord, Generic, NFData)

mkSymbol :: BSL.ByteString -> FixityNode ()
mkSymbol = Symbol ()

mkIdent :: BSL.ByteString -> FixityNode ()
mkIdent = FixityIdent ()

-- from: https://github.com/githwxi/ATS-Xanadu/blob/master/prelude/fixity.sats
defaultFixity :: SymEnv
defaultFixity = M.fromList
    [ (mkSymbol "!", Prefix 99)
    , (mkSymbol "**", Infixr 61)
    , (mkSymbol "*", Infixl 60)
    , (mkSymbol "/", Infixl 60)
    , (mkSymbol "%", Infixl 60)
    , (mkIdent "mod", Infixl 60)
    , (mkSymbol "~", Prefix 51)
    -- TODO: + and - overloading
    , (mkIdent "asl", Infixl 41)
    , (mkIdent "asr", Infixl 41)
    , (mkIdent "lsl", Infixl 41)
    , (mkIdent "lsr", Infixl 41)
    , (mkSymbol "::", Infixr 40)
    , (mkSymbol "@", Infixr 40)
    , (mkSymbol "<", Infix0 40)
    , (mkSymbol "<=", Infix0 40)
    , (mkSymbol ">", Infix0 40)
    , (mkSymbol ">=", Infix0 40)
    , (mkSymbol "=", Infix0 30)
    , (mkSymbol "!=", Infix0 30)
    , (mkSymbol "==", Infix0 30)
    , (mkSymbol "!==", Infix0 30)
    , (mkSymbol "||", Infixl 20)
    , (mkSymbol "&&", Infixl 21)
    , (mkIdent "andalso", Infixl 20)
    , (mkIdent "land", Infixl 20)
    , (mkIdent "orelse", Infixl 21)
    , (mkIdent "xor", Infixl 21)
    , (mkIdent "lor", Infixl 21)
    , (mkIdent "lxor", Infixl 21)
    , (mkSymbol "->", Infixr 10)
    , (mkSymbol ":=", Infix0 0)
    , (mkSymbol ":=:", Infix0 0)
    , (mkSymbol "<<", Infixl 0)
    , (mkSymbol ">>", Infixr 0)
    ]
