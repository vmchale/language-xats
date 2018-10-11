module Language.XATS
    ( exec
    , head'
    ) where

head' :: [a] -> Maybe a
head' []    = Nothing
head' (x:_) = Just x

exec :: IO ()
exec = putStrLn "language-xats from template"
