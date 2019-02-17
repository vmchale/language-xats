-- vim: syntax=hspec
{-# LANGUAGE OverloadedStrings #-}

import           Language.XATS
import           Test.Hspec

main :: IO ()
main = hspec $ parallel $
    describe "getDeps" $
        it "works on a trivial example" $
            getDeps "test/deps/file.dats" >>= (`shouldBe` ["test/deps/file.sats"])
