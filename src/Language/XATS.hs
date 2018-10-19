module Language.XATS
    ( head'
    ) where

head' :: [a] -> Maybe a
head' []    = Nothing
head' (x:_) = Just x
