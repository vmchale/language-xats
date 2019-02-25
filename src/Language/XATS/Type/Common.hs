-- | This module is parallel to
-- [this](https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/SATS/basics.sats)
-- file.
module Language.XATS.Type.Common ( DeclKind (..)
                                 , ValKind (..)
                                 , FunKind (..)
                                 , ImpKind (..)
                                 , FunClosure (..)
                                 ) where

-- | Defined
-- [here](https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/SATS/basics.sats#L141)
data DeclKind = DKfun
              | DKval
              | DKpraxi
              | DKprfun
              | DKprval
              | DKcastfn

-- | Defined
-- [here](https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/SATS/basics.sats#L159)
data ValKind = VKval
             | VKvalp -- ^ @val+@
             | VKvaln -- ^ @val-@
             | VKprval

-- | Defined
-- [here](https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/SATS/basics.sats#L177)
data FunKind = FKfn0 -- ^ Nonrecursive
             | FKfnx -- ^ Tail-recursive
             | FKfn1 -- ^ Recursive
             | FKfun -- ^ Recursive
             | FKprfn0 -- ^ Nonrecursive proof function
             | FKprfn1 -- ^ Recursive proof function
             | FKprfun -- ^ Recursive proof function
             | FKpraxi -- ^ Proof axoim
             | FKcastfn -- ^ Cast

-- | Defined
-- [here](https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/SATS/basics.sats#L200)
data ImpKind = IKprf -- ^ proof implementation
             | IKval -- ^ value implementation

-- | Defined
-- [here](https://github.com/githwxi/ATS-Xanadu/blob/master/srcgen/xats/SATS/basics.sats#L220)
data FunClosure = FCfun
                | FCcloflt -- ^ Flat
                | FCcloptr -- ^ Linear, boxed
                | FCcloref -- ^ Nonlinear, boxed
