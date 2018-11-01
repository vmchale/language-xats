{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Criterion.Main
import qualified Data.ByteString.Lazy as BSL
import           Language.XATS

tinyProgram :: BSL.ByteString
tinyProgram = "#prefix 99 !"

-- TODO: benchmark dependency extraction

main :: IO ()
main =
    defaultMain [ bgroup "parse"
                      [ bench "parse" $ nf parse tinyProgram ]
                , bgroup "dependencie"
                      [ bench "getDeps" $ nfIO (getDeps "test/deps/file.dats")
                      , bench "getAll" $ nfIO (getAll "test/deps/file.dats")
                      ]
                ]
