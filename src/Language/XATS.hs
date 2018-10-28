module Language.XATS
    ( -- * AST Types
      XATS (..)
    , Declaration (..)
    , FixityD (..)
    , FixityRes (..)
    , FixityNode (..)
    -- * Error types
    , ParseError
    , LexerError
    -- * Lexer types
    , AlexPosn
    , Token
    -- * Parsing
    , parse
    -- * Lexing
    , lexXATS
    -- * Dependency analysis
    , getDeps
    ) where

import           Language.XATS.Dependency
import           Language.XATS.Lexer
import           Language.XATS.Parser
import           Language.XATS.Type
