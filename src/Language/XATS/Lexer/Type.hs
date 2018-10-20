-- Corresponding file: https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/SATS/lexing.sats
--
-- This is incomplete and thus the above file contains various improvements of
-- the module below.

module Language.XATS.Lexer.Type ( Token (..)
                                , Addendum (..)
                                , Special (..)
                                , Keyword (..)
                                , LambdaAdd (..)
                                , FunKind (..)
                                , ImplKind (..)
                                ) where

import qualified Data.ByteString.Lazy as BSL

data Addendum = None
              | Plus
              | Minus

-- | Used to distinguish @fix\@@ from @fix@
data LambdaAdd = NoneLam
               | AtLam

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
             | EqGt -- ^ @=>@
             | LtGt -- ^ @<>@
             | GtLt -- ^ @><@
             | MinusLt -- ^ @-<@
             | MinusGt -- ^ @->@
             | GtDot -- ^ @>.@
             | Comma -- ^ @,@
             | Semicolon -- ^ @;@
             | Backslash -- ^ @\\@
             | LParen -- ^ @(@
             | RParen -- ^ @)@
             | LBrace -- ^ @{@
             | RBrace -- ^ @}@
             | LBracket -- ^ @[@
             | RBracket -- ^ @]@

data FunKind = Fn0
             | Fnx
             | Fn1
             | Fun
             | PrFn0
             | PrFn1
             | PrFun
             | Praxi
             | CastFn

data ImplKind = Impl
              | PrImpl

data Keyword = As
             | Of
             | Op -- ^ @op@
             | In
             | And
             | End
             | If
             | Sif
             | Then
             | Else
             | When
             | With
             | Case Addendum
             | SCase
             | EndIf
             | EndsIf
             | EndCase
             | Lam LambdaAdd
             | Fix LambdaAdd
             | Let
             | Where
             | Local
             | EndLam
             | EndLet
             | EndWhere
             | EndLocal
             | Val Addendum
             | PrVal
             | Var
             | FunTok FunKind
             | ImplTok ImplKind
             | SortDef
             | SExpDef -- @sexpdef@
             | PropDef
             | ViewDef
             | TypeDef
             | ViewTypeDef
             | AbsProp
             | AbsView
             | AbsType
             | AbsTBox
             | AbsTFlat
             | AbsViewType
             | AbsVTBox
             | AbsVTFlat
             | AbsImpl
             | AbsOpen
             | DataSort
             | DataProp
             | DataView
             | DataType
             | DataViewType
             | WithType
             | WithProp
             | WithView
             | WithViewType
             | Tuple -- ^ @$tup@
             | TupleType -- ^ @$tup_t@
             | TupleViewType -- ^ @$tup_vt@
             | Rec -- ^ @$rec@
             | RecType -- ^ @$rec_t@
             | RecViewType -- ^ @$rec_vt@
             | Infix
             | Infix0
             | Infixl
             | Infixr
             | Prefix
             | Postfix
             | Nonfix -- ^ @#nonfix@
             | Stacst -- ^ @#stacst@
             | Static -- ^ @#static@
             | Extern -- ^ @#extern@
             | Include -- ^ @#include@
             | Staload -- ^ @#staload@
             | Dynload -- ^ @#dynload@
             | Symload -- ^ @#symload@

data Token a = EOF { loc :: a }

             | IdentAlpha { loc :: a, ident :: BSL.ByteString } -- ^ Alphanumeric identifier
             | IdentSym { loc :: a, ident :: BSL.ByteString } -- ^ Symbol
             | IdentOctothorpe { loc :: a, ident :: BSL.ByteString } -- ^ Identifier with @#@ in front
             | IdentDollar { loc :: a, ident :: BSL.ByteString } -- ^ Identifier with a @$@ in front

             | TokInt { loc :: a, intStr :: Integer } -- ^ Base 10 integer

             | TokKeyword a Keyword
             | TokSpecial a Special
