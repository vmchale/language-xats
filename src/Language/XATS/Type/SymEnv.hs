{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Language.XATS.Type.SymEnv ( FixityNode (..)
                            , SymEnv
                            ) where

import           Control.DeepSeq      (NFData)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map             as M
import           GHC.Generics         (Generic)
import           GHC.Word             (Word8)

type SymEnv = M.Map (FixityNode ()) Word8

data FixityNode a = Symbol a BSL.ByteString
                  | FixityIdent a BSL.ByteString
                  deriving (Eq, Ord, Generic, NFData)

