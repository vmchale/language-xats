{-# LANGUAGE OverloadedStrings #-}

import           Language.XATS
import           Test.Hspec

main :: IO ()
main = hspec $ parallel $
    describe "parse" $
        it "works on prefix" $
            parse "#prefix 99 !" `shouldBe` Right (XATS [])
