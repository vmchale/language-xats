{-# LANGUAGE OverloadedStrings #-}

-- | Dependency analysis for ATS.
module Language.XATS.Dependency ( getDeps
                                ) where

import           Control.Monad            (filterM)
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.Text                as T
import           Language.XATS.Lexer
import           Language.XATS.Type.Lexer
import           System.Directory         (doesFileExist)
import           System.Exit              (exitFailure)
import           System.FilePath          (takeDirectory)

-- | Get immediate dependencies of a file
getDeps :: FilePath
        -> IO [FilePath]
getDeps fp = do
    contents' <- BSL.readFile fp
    let dot = takeDirectory fp
        dotdot = takeDirectory dot
        proc = T.replace "../" (T.pack (dotdot ++ "/")) . T.replace "./" (T.pack (dot ++ "/"))
    processed <- either prErr pure (extractDeps contents')
    let procFps = proc <$> processed
    filterM doesFileExist (T.unpack <$> procFps)

prErr :: LexerError -> IO a
prErr = (*> exitFailure) . putStrLn

extractDeps :: BSL.ByteString
            -> Either LexerError [T.Text]
extractDeps = fmap extractTokDeps . lexXATS
    where extractTokDeps []                                              = []
          extractTokDeps (TokKeyword _ Include : TokString _ str : toks) = str : extractTokDeps toks
          extractTokDeps (TokKeyword _ Staload : TokString _ str : toks) = str : extractTokDeps toks
          extractTokDeps (TokKeyword _ Dynload : TokString _ str : toks) = str : extractTokDeps toks
          extractTokDeps (TokKeyword _ Symload : TokString _ str : toks) = str : extractTokDeps toks
          extractTokDeps (_ : toks)                                      = extractTokDeps toks
