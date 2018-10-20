{
    module Language.XATS.Parser ( parse
                                , ParseError (..)
                                ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Lazy as BSL
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

%%

many(p)
    : many(p) p { $2 : $1 }
    | { [] }

some(p)
    : some(p) p { $2 :| toList $1 }
    | p { $1 :| [] }

parens(p)
    : openParen p closeParen { $2 }

XATS : many(Declaration) { XATS $1 }

Declaration : { undefined }

{

parseError :: Token AlexPosn -> Parse a
parseError = throwError . Unexpected

data ParseError a = Unexpected (Token a)
                  | LexErr String

type Parse = ExceptT (ParseError AlexPosn) Alex

parse :: BSL.ByteString -> Either (ParseError AlexPosn) (XATS a)
parse str = liftErr $ runAlex str (runExceptT parseXATS)
    where liftErr (Left err) = Left (LexErr err)
          liftErr (Right (Left err)) = Left err
          liftErr (Right (Right x)) = Right x

}
