module Language.XATS.Lexer.Type ( Token (..)
                                , Addendum (..)
                                , Special (..)
                                ) where

import qualified Data.ByteString.Lazy as BSL

data Addendum = None
              | Plus
              | Minus

data Special = DotLT -- ^ @.<@
             | ColonLT -- ^ @:<@
             | At
             | VBar -- ^ @|@
             | Dot -- ^ @.@
             | Colon -- ^ @:@
             | Eq -- ^ @=@
             | Lt -- ^ @<@
             | Gt -- ^ @>@
             | Dollar -- ^ @$@
             | Octothorpe -- @#@
             | LtEq -- ^ @<=@
             | GtEq -- ^ @>=@
             | LtGt -- ^ @<>@
             | GtLt -- ^ @><@
             | MinusLt -- ^ @-<@
             | GtDot -- ^ @>.@
             | Comma -- ^ @,@
             | Semicolon -- ^ @;@
             | Backslash -- ^ @\\@

data Token a = EOF { loc :: a }
             | IdentAlpha { loc :: a, ident :: BSL.ByteString } -- ^ Alphanumeric identifier
             | IdentSym { loc :: a, ident :: BSL.ByteString } -- ^ Symbol
             | IdentOctothorpe { loc :: a, ident :: BSL.ByteString } -- ^ Identifier with @#@ in front
             | IdentDollar { loc :: a, ident :: BSL.ByteString } -- ^ Identifier with a @$@ in front
             | TokInt1 { loc :: a, intStr :: BSL.ByteString } -- ^ Base 10 integer
             | TokInt2 { loc :: a, intStr :: BSL.ByteString, base :: Int } -- ^ Base n integer
             | TokInt3 { loc :: a, intStr :: BSL.ByteString, base :: Int, suffix :: Word } -- ^ Base n integer w/ suffix
             | TokFloat1 { loc :: a, floatStr :: BSL.ByteString } -- ^ Base 10
             | TokFloat2 { loc :: a, floatStr :: BSL.ByteString, base :: Int }
             | TokFloat3 { loc :: a, floatStr :: BSL.ByteString, base :: Int, suffix :: Word }
             | TokCharNil { loc :: a } -- ^ @''@
             | TokChar { loc :: a, char :: Char } -- ^ '?'
             | TokSpecialChar { loc :: a, char :: Char }
