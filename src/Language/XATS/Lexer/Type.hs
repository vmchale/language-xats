-- Corresponding file: https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/SATS/lexing.sats

{-# LANGUAGE OverloadedStrings #-}
module Language.XATS.Lexer.Type ( Token (..)
                                , Addendum (..)
                                , Special (..)
                                , Keyword (..)
                                , LambdaAdd (..)
                                , FunKind (..)
                                , ImplKind (..)
                                ) where

import qualified Data.ByteString.Lazy      as BSL
import           Data.Text.Prettyprint.Doc

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
             | Case { addendum :: Addendum }
             | SCase
             | EndIf
             | EndsIf
             | EndCase
             | Lam { lambdaAdd :: LambdaAdd }
             | Fix { lambdaAdd :: LambdaAdd }
             | Let
             | Where
             | Local
             | EndLam
             | EndLet
             | EndWhere
             | EndLocal
             | Val { addendum :: Addendum }
             | PrVal
             | Var
             | FunTok { funkind :: FunKind }
             | ImplTok { implKind :: ImplKind }
             | SortDef
             | StaExpDef -- @sexpdef@
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
             | Tup -- ^ @$tup@
             | TupType -- ^ @$tup_t@
             | TupViewType -- ^ @$tup_vt@
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

             | TokKeyword { loc :: a, keyword :: Keyword }
             | TokSpecial { loc :: a, special :: Special }

instance Pretty Addendum where
    pretty Plus  = "+"
    pretty Minus = "-"
    pretty None  = ""

instance Pretty LambdaAdd where
    pretty NoneLam = ""
    pretty AtLam   = "@"

instance Pretty FunKind where
    pretty Fn0    = "fn0"
    pretty Fnx    = "fnx"
    pretty Fn1    = "fn1"
    pretty Fun    = "fun"
    pretty PrFn0  = "prfn0"
    pretty PrFn1  = "prfn1"
    pretty PrFun  = "prfun"
    pretty Praxi  = "praxi"
    pretty CastFn = "castfn"

instance Pretty ImplKind where
    pretty Impl   = "implement"
    pretty PrImpl = "primplmnt"

instance Pretty Keyword where
    pretty As           = "as"
    pretty Of           = "of"
    pretty Op           = "op"
    pretty In           = "in"
    pretty And          = "and"
    pretty End          = "end"
    pretty If           = "if"
    pretty Sif          = "sif"
    pretty Then         = "then"
    pretty Else         = "else"
    pretty When         = "when"
    pretty With         = "with"
    pretty (Case add)   = "case" <> pretty add
    pretty SCase        = "scase"
    pretty EndIf        = "endif"
    pretty EndsIf       = "endsif"
    pretty EndCase      = "endcase"
    pretty (Lam mAt)    = "lam" <> pretty mAt
    pretty (Fix mAt)    = "fix" <> pretty mAt
    pretty Let          = "let"
    pretty Where        = "where"
    pretty Local        = "local"
    pretty EndLam       = "endlam"
    pretty EndLet       = "endlet"
    pretty EndWhere     = "endwhere"
    pretty EndLocal     = "endlocal"
    pretty (Val add)    = "val" <> pretty add
    pretty PrVal        = "prval"
    pretty Var          = "var"
    pretty (FunTok f)   = pretty f
    pretty (ImplTok i)  = pretty i
    pretty SortDef      = "sortdef"
    pretty StaExpDef    = "sexpdef"
    pretty PropDef      = "propdef"
    pretty ViewDef      = "viewdef"
    pretty TypeDef      = "typedef"
    pretty ViewTypeDef  = "vtypedef"
    pretty AbsProp      = "absprop"
    pretty AbsView      = "absview"
    pretty AbsType      = "abstype"
    pretty AbsTBox      = "abstbox"
    pretty AbsTFlat     = "abstflat"
    pretty AbsViewType  = "absvtype"
    pretty AbsVTBox     = "absvtbox"
    pretty AbsVTFlat    = "absvtflat"
    pretty AbsImpl      = "absimpl"
    pretty AbsOpen      = "absopen"
    pretty DataSort     = "datasort"
    pretty DataProp     = "dataprop"
    pretty DataView     = "dataview"
    pretty DataType     = "datatype"
    pretty DataViewType = "datavtype"
    pretty WithType     = "withtype"
    pretty WithProp     = "withprop"
    pretty WithView     = "withview"
    pretty WithViewType = "withvtype"
    pretty Tup          = "$tup"
    pretty TupType      = "$tup_t"
    pretty TupViewType  = "$tup_vt"
    pretty Rec          = "$rec"
    pretty RecType      = "$rec_t"
    pretty RecViewType  = "$rec_vt"
    pretty Infix        = "#infix"
    pretty Infix0       = "#infix0"
    pretty Infixl       = "#infixl"
    pretty Infixr       = "#infixr"
    pretty Prefix       = "#prefix"
    pretty Postfix      = "#postfix"
    pretty Nonfix       = "#nonfix"
    pretty Stacst       = "#stacst"
    pretty Static       = "#static"
    pretty Extern       = "#extern"
    pretty Include      = "#include"
    pretty Staload      = "#staload"
    pretty Dynload      = "#dynload"
    pretty Symload      = "#symload"
