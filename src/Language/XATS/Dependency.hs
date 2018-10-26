module Language.XATS.Dependency ( extractDeps
                                ) where

import qualified Data.ByteString.Lazy as BSL
import           Language.XATS.Lexer

extractDeps :: BSL.ByteString
            -> Either LexerError [BSL.ByteString]
extractDeps = fmap extractTokDeps . lexXATS

extractTokDeps :: [Token AlexPosn] -> [BSL.ByteString]
extractTokDeps _ = mempty
