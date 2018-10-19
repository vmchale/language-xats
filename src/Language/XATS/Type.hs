module Language.XATS.Type ( Declaration
                          , XATS (..)
                          ) where

newtype XATS a = XATS [Declaration a]

data Declaration a
