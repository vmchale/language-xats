{
    {-# LANGUAGE DeriveAnyClass #-}
    {-# LANGUAGE DeriveGeneric  #-}
    module Language.XATS.Parser ( parse
                                , ParseError (..)
                                ) where

import Control.DeepSeq (NFData)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics (Generic)
import Language.XATS.Lexer.Type
import Language.XATS.Lexer
import Language.XATS.Type

}

%name parseXATS XATS
%tokentype { Token AlexPosn }
%error { parseError }
%monad { Parse } { (>>=) } { pure }
%lexer { lift alexMonadScan >>= } { EOF _ }

%token

    lparen { TokSpecial $$ LParen }
    rparen { TokSpecial $$ RParen }

    as { TokKeyword $$ As }
    of { TokKeyword $$ Of }
    op { TokKeyword $$ Op }
    in { TokKeyword $$ In }
    and { TokKeyword $$ And }
    end { TokKeyword $$ End }
    if { TokKeyword $$ If }
    sif { TokKeyword $$ Sif }
    then { TokKeyword $$ Then }
    else { TokKeyword $$ Else }
    when { TokKeyword $$ When }
    with { TokKeyword $$ With }
    case { $$@(TokKeyword _ Case{}) }
    scase { TokKeyword $$ SCase }
    endif { TokKeyword $$ EndIf }
    endcase { TokKeyword $$ EndCase }

    prefix { TokKeyword $$ KwPrefix }
    infixl { TokKeyword $$ KwInfixl }
    infixr { TokKeyword $$ KwInfixr }
    infix0 { TokKeyword $$ KwInfix0 }
    postfix { TokKeyword $$ KwPostfix }
    nonfix { TokKeyword $$ KwNonfix }

    intLit { $$@TokInt{} }

    symIdent { $$@IdentSym{} }

%%

many(p)
    : many(p) p { $2 : $1 }
    | { [] }

some(p)
    : many(p) p { $2 :| $1 }

parens(p)
    : lparen p rparen { $2 }

XATS : many(Declaration) { XATS $1 }

-- TODO: this should modify the internal user state.
FixityRes : intLit { IntFix (loc $1) (fromIntegral $ intStr $1) }
          | parens(intLit) { IntFix (loc $1) (fromIntegral $ intStr $1) }

FixityNode : symIdent { Symbol (loc $1) (ident $1) }

Fixity : FixityRes some(FixityNode) { FixityD $1 $2 }

Declaration : prefix Fixity { PrefixDecl $1 $2 }
            | infixl Fixity { InfixlDecl $1 $2 }

{

parseError :: Token AlexPosn -> Parse a
parseError = throwError . Unexpected

data ParseError a = Unexpected (Token a)
                  | LexErr String
                  deriving (Generic, NFData)

type Parse = ExceptT (ParseError AlexPosn) Alex

parse :: BSL.ByteString -> Either (ParseError AlexPosn) (XATS AlexPosn)
parse str = liftErr $ runAlex str (runExceptT parseXATS)
    where liftErr (Left err) = Left (LexErr err)
          liftErr (Right (Left err)) = Left err
          liftErr (Right (Right x)) = Right x

}
