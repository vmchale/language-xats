module Language.XATS.Rewrite ( rewriteExpr
                             ) where

import           Language.XATS.Type

-- | Rewrite a parsed expression to respect operator precedence
rewriteExpr :: SymEnv -> Expression a -> Expression a
rewriteExpr _ = id
