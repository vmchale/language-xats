{-# LANGUAGE OverloadedStrings #-}

import           Language.XATS
import           Test.Hspec

main :: IO ()
main = hspec $ parallel $
    describe "extractDeps" $
        it "works on a trivial example" $
            extractDeps "#prefix 99 !\n#symload \"file.sats\"" `shouldBe` Right ["file.sats"]
