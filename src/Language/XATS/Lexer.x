{

    {-# OPTIONS_GHC -fno-warn-unused-imports #-}
    {-# LANGUAGE DeriveGeneric      #-}
    {-# LANGUAGE DeriveAnyClass     #-}
    {-# LANGUAGE OverloadedStrings  #-}
    {-# LANGUAGE StandaloneDeriving #-}

    module Language.XATS.Lexer ( alexMonadScan
                               , runAlex
                               , lexXATS
                               -- * Types
                               , AlexPosn (..)
                               , Alex (..)
                               , LexerError
                               ) where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.DeepSeq (NFData)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as ASCII
import Data.Functor (($>))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Language.XATS.Type.Lexer
import Language.XATS.Type.SymEnv

}

%wrapper "monadUserState-bytestring"

$digit = [0-9]
$hexit = [0-9 a-f A-F]
$lower = [a-z]
$upper = [A-Z]
$alpha = [$lower $upper]

@sign = "-" | ""

@integer = @sign $digit+

-- loosely based on: https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/DATS/lexing_util0.dats#L1245
@escape_str = \\ [\\\"]

-- see: https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/DATS/lexing_util0.dats#L174
$identifier_char = [$alpha $digit \_ \' \$]
@identifier = ($lower | $upper | \_) $identifier_char*

@string = \" ([^\"\\] | @escape_str)* \"

-- see: https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/DATS/lexing_util0.dats#L184
@symbol = [\!\&\*\/\%\-\~\<\>\=\:\@\|\?\#\`\+\^]+

tokens :-
    
    <0> $white+                  ;
    <0> "//".*                   { tok (\p s -> alex $ LineComment p s) }
    <0> "(*"                     { \_ _ -> ml_nested_comment }
    <0> "/*"                     { \_ _ -> c_nested_comment }
    -- TODO: nested comments

    <0> ":<"                     { mkSpecial ColonLT }
    <0> \@                       { mkSpecial At }
    <0> \|                       { mkSpecial VBar }
    <0> \.                       { mkSpecial Dot }
    <0> \:                       { mkSpecial Colon }
    <0> \=                       { mkSpecial Eq }
    <0> \<                       { mkSpecial Lt }
    <0> \>                       { mkSpecial Gt }
    <0> \$                       { mkSpecial Dollar }
    <0> \#                       { mkSpecial Octothorpe }
    <0> "<="                     { mkSpecial LtEq }
    <0> ">="                     { mkSpecial GtEq }
    <0> "=>"                     { mkSpecial EqGt }
    <0> "<>"                     { mkSpecial LtGt }
    <0> "><"                     { mkSpecial GtLt }
    <0> "-<"                     { mkSpecial MinusLt }
    <0> ">."                     { mkSpecial GtDot }
    <0> \,                       { mkSpecial Comma }
    <0> \;                       { mkSpecial Semicolon }
    <0> \\                       { mkSpecial Backslash }
    <0> \(                       { mkSpecial LParen }
    <0> \)                       { mkSpecial RParen }
    <0> \{                       { mkSpecial LBrace }
    <0> \}                       { mkSpecial RBrace }
    <0> \[                       { mkSpecial LBracket }
    <0> \]                       { mkSpecial RBracket }

    -- keywords are defined here: https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/DATS/lexing_kword.dats
    <0> "fn0"                    { mkFun Fn0 }
    <0> fnx                      { mkFun Fnx }
    <0> "fn1"                    { mkFun Fn1 }
    <0> fun                      { mkFun Fun }
    <0> "prfn0"                  { mkFun PrFn0 }
    <0> "prfn1"                  { mkFun PrFn1 }
    <0> prfun                    { mkFun PrFun }
    <0> praxi                    { mkFun Praxi }
    <0> castfn                   { mkFun CastFn }

    <0> as                       { mkKeyword As }
    <0> of                       { mkKeyword Of }
    <0> op                       { mkKeyword Op }
    <0> in                       { mkKeyword In }
    <0> and                      { mkKeyword And }
    <0> end                      { mkKeyword End }
    <0> if                       { mkKeyword If }
    <0> sif                      { mkKeyword Sif }
    <0> then                     { mkKeyword Then }
    <0> else                     { mkKeyword Else }
    <0> when                     { mkKeyword When }
    <0> with                     { mkKeyword With }

    <0> "case+"                  { mkKeyword (Case Plus) }
    <0> "case-"                  { mkKeyword (Case Minus) }
    <0> "case"                   { mkKeyword (Case None) }

    <0> scase                    { mkKeyword SCase }
    <0> endif                    { mkKeyword EndIf }
    <0> endsif                   { mkKeyword EndsIf }
    <0> endcase                  { mkKeyword EndCase }

    <0> "lam"                    { mkKeyword (Lam NoneLam) }
    <0> "lam@"                   { mkKeyword (Lam AtLam) }
    <0> "fix"                    { mkKeyword (Fix NoneLam) }
    <0> "fix@"                   { mkKeyword (Fix AtLam) }

    <0> let                      { mkKeyword Let }
    <0> where                    { mkKeyword Where }
    <0> local                    { mkKeyword Local }
    <0> endlam                   { mkKeyword EndLam }
    <0> endlet                   { mkKeyword EndLet }
    <0> endwhere                 { mkKeyword EndWhere }
    <0> endlocal                 { mkKeyword EndLocal }

    <0> "val+"                   { mkKeyword (Val Plus) }
    <0> "val-"                   { mkKeyword (Val Minus) }
    <0> val                      { mkKeyword (Val None) }
    <0> var                      { mkKeyword Var }
    <0> prval                    { mkKeyword PrVal }

    <0> implmnt                  { mkKeyword (ImplTok Impl) }
    <0> implement                { mkKeyword (ImplTok Impl) }
    <0> primplmnt                { mkKeyword (ImplTok PrImpl) }
    <0> primplement              { mkKeyword (ImplTok PrImpl) }

    <0> sortdef                  { mkKeyword SortDef }
    <0> sexpdef                  { mkKeyword StaExpDef }
    <0> propdef                  { mkKeyword PropDef }
    <0> viewdef                  { mkKeyword ViewDef }
    <0> typedef                  { mkKeyword TypeDef }
    <0> vtypedef                 { mkKeyword ViewTypeDef }

    <0> absprop                  { mkKeyword AbsProp }
    <0> absview                  { mkKeyword AbsView }

    <0> abstype                  { mkKeyword AbsType }
    <0> abstbox                  { mkKeyword AbsTBox }
    <0> abstflt                  { mkKeyword AbsTFlat }
    <0> abstflat                 { mkKeyword AbsTFlat }

    <0> absvtype                 { mkKeyword AbsViewType }
    <0> absvtbox                 { mkKeyword AbsVTBox }
    <0> absvtflt                 { mkKeyword AbsVTFlat }
    <0> absvtflat                { mkKeyword AbsVTFlat }

    <0> absimpl                  { mkKeyword AbsImpl }
    <0> absopen                  { mkKeyword AbsOpen }

    <0> datasort                 { mkKeyword DataSort }
    <0> dataprop                 { mkKeyword DataProp }
    <0> dataview                 { mkKeyword DataView }
    <0> datatype                 { mkKeyword DataType }
    <0> datavtype                { mkKeyword DataViewType }

    <0> withtype                 { mkKeyword WithType }
    <0> withprop                 { mkKeyword WithProp }
    <0> withview                 { mkKeyword WithView }
    <0> withvtype                { mkKeyword WithViewType }

    <0> "$tup"                   { mkKeyword Tup }
    <0> "$tup_t"                 { mkKeyword TupType }
    <0> "$tup_vt"                { mkKeyword TupViewType }

    <0> "$rec"                   { mkKeyword Rec }
    <0> "$rec_t"                 { mkKeyword RecType }
    <0> "$rec_vt"                { mkKeyword RecViewType }

    <0> "#infix"                 { mkKeyword KwInfix }
    <0> "#infix0"                { mkKeyword KwInfix0 }
    <0> "#infixl"                { mkKeyword KwInfixl }
    <0> "#infixr"                { mkKeyword KwInfixr }
    <0> "#prefix"                { mkKeyword KwPrefix }
    <0> "#postfix"               { mkKeyword KwPostfix }
    <0> "#nonfix"                { mkKeyword KwNonfix }

    <0> "#stacst"                { mkKeyword Stacst }
    <0> "#static"                { mkKeyword Static }
    <0> "#extern"                { mkKeyword Extern }
    <0> "#include"               { mkKeyword Include }
    <0> "#staload"               { mkKeyword Staload }
    <0> "#dynload"               { mkKeyword Dynload }
    <0> "#symload"               { mkKeyword Symload }

    <0> @identifier              { tok (\p s -> alex $ IdentAlpha p s) }
    <0> \# @identifier           { tok (\p s -> alex $ IdentOctothorpe p (BSL.drop 1 s)) }
    <0> \$ @identifier           { tok (\p s -> alex $ IdentDollar p (BSL.drop 1 s)) }
    <0> @symbol                  { tok (\p s -> alex $ IdentSym p s) }

    <0> @string                  { tok (\p s -> alex $ TokString p (strProcess s)) }

    <0> @integer                 { tok (\p s -> alex $ TokInt p (readBSL s)) }

{

deriving instance Generic AlexPosn
deriving instance NFData AlexPosn

strProcess :: BSL.ByteString -> T.Text
strProcess = replacements . decodeUtf8 . BSL.toStrict . BSL.init . BSL.drop 1
    where replacements = T.replace "\\\\" "\\" . T.replace "\\\"" "\""

ml_nested_comment = nested_comment 40 41

c_nested_comment = nested_comment 47 47

-- Taken from example by Simon Marlow.
-- This handles nested comments
nested_comment :: Word8 -> Word8 -> Alex (Token AlexPosn)
nested_comment c1 c2 = go 1 [] =<< alexGetInput

    where go :: Int -> [Word8] -> AlexInput -> Alex (Token AlexPosn)
          go 0 cs input = do
            alexSetInput input
            BlockComment <$> get_pos <*> pure (BSL.reverse (BSL.pack cs))
          go n cs input =
            case alexGetByte input of
                Nothing -> err input
                Just (c, input') ->
                    case c of
                        42 ->
                            case alexGetByte input' of
                                Nothing -> err input'
                                Just (c',input'') | c' == c2 -> go (n-1) (c:cs) input''
                                Just (_,input'') -> go n (c:cs) input''
                        _ | c == c1 ->
                            case alexGetByte input' of
                                Nothing -> err input'
                                Just (c',input'') -> go (addLevel c' $ n) (c:cs) input''
                        _ -> go n (c:cs) input'

          addLevel c' = if c'== 42 then (+1) else id

          err (pos,_,_,_) =
            let (AlexPn _ line col) = pos in
                alexError ("Error in nested comment at line " ++ show line ++ ", column " ++ show col)

readBSL :: Read a => BSL.ByteString -> a
readBSL = read . ASCII.unpack

data ParseMode = StaticParser
               | DynamicParser

type AlexUserState = (ParseMode, SymEnv)

alexInitUserState :: AlexUserState
alexInitUserState = (DynamicParser, mempty)

gets_alex :: (AlexState -> a) -> Alex a
gets_alex f = Alex (Right . (id &&& f))

get_pos :: Alex AlexPosn
get_pos = gets_alex alex_pos

alexEOF = EOF <$> get_pos

alex :: a -> Alex a
alex = pure

tok f (p,_,s,_) len = f p (BSL.take len s)

constructor c t = tok (\p _ -> alex $ c p t)

mkFun = mkKeyword . FunTok

mkSpecial = constructor TokSpecial

mkKeyword = constructor TokKeyword

loop :: Alex [Token AlexPosn]
loop = do
    tok' <- alexMonadScan
    case tok' of
        EOF{} -> pure []
        _     -> (tok' :) <$> loop

type LexerError = String

lexXATS :: BSL.ByteString -> Either LexerError [Token AlexPosn]
lexXATS = flip runAlex loop

}
