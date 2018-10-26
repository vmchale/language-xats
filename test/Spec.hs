{-# LANGUAGE OverloadedStrings #-}

import           Language.XATS
import           Test.Hspec

main :: IO ()
main = hspec $ parallel $
    describe "extractDeps" $
        it "works" $
            extractDeps "#symload \'file.sats\"" `shouldBe` Right ["file.sats"]
