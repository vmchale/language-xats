-- | Dependency analysis for ATS.
module Language.XATS.Dependency ( extractDeps
                                ) where

import qualified Data.ByteString.Lazy     as BSL
import qualified Data.Text                as T
import           Language.XATS.Lexer
import           Language.XATS.Type.Lexer

extractDeps :: BSL.ByteString
            -> Either LexerError [T.Text]
extractDeps = fmap extractTokDeps . lexXATS

extractTokDeps :: [Token AlexPosn] -> [T.Text]
extractTokDeps []                                              = []
extractTokDeps (TokKeyword _ Include : TokString _ str : toks) = str : extractTokDeps toks
extractTokDeps (TokKeyword _ Staload : TokString _ str : toks) = str : extractTokDeps toks
extractTokDeps (TokKeyword _ Dynload : TokString _ str : toks) = str : extractTokDeps toks
extractTokDeps (TokKeyword _ Symload : TokString _ str : toks) = str : extractTokDeps toks
extractTokDeps (_ : toks)                                      = extractTokDeps toks
