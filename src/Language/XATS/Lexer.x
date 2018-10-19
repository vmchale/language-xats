{

    {-# OPTIONS_GHC -fno-warn-unused-imports #-}
    module Language.XATS.Lexer ( alexMonadScan
                               , runAlex
                               -- * Types
                               , AlexPosn (..)
                               , Alex (..)
                               ) where

import Control.Arrow ((&&&))
import qualified Data.ByteString.Lazy as BSL
import Language.XATS.Lexer.Type

}

%wrapper "monadUserState-bytestring"

$digit = [0-9]
$lower = [a-z]
$upper = [A-Z]

@sign = "-" | ""

@integer = @sign $digit+

tokens :-
    
    <0> $white+                  ;
    <0> "//".*                   ;
    -- TODO: nested comments

    <0> as                       { mkKeyword As }

{

type AlexUserState = ()

alexInitUserState :: AlexUserState
alexInitUserState = ()

gets_alex :: (AlexState -> a) -> Alex a
gets_alex f = Alex (Right . (id &&& f))

get_pos :: Alex AlexPosn
get_pos = gets_alex alex_pos

alexEOF = EOF <$> get_pos

alex :: a -> Alex a
alex = pure

tok f (p,_,s,_) len = f p (BSL.take len s)

constructor c t = tok (\p _ -> alex $ c p t)

mkKeyword = constructor TokKeyword

}
