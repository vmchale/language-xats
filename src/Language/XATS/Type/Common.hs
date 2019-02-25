module Language.XATS.Type.Common ( DeclKind (..)
                                 , ValKind (..)
                                 , FunKind (..)
                                 , ImpKind (..)
                                 , FunClosure (..)
                                 ) where

data DeclKind = DKfun
              | DKval
              | DKpraxi
              | DKprfun
              | DKprval
              | DKcastfn

data ValKind = VKval
             | VKvalp -- ^ @val+@
             | VKvaln -- ^ @val-@
             | VKprval

data FunKind = FKfn0 -- ^ Nonrecursive
             | FKfnx -- ^ Tail-recursive
             | FKfn1 -- ^ Recursive
             | FKfun -- ^ Recursive
             | FKprfn0 -- ^ Nonrecursive proof function
             | FKprfn1 -- ^ Recursive proof function
             | FKprfun -- ^ Recursive proof function
             | FKpraxi -- ^ Proof axoim
             | FKcastfn -- ^ Cast

data ImpKind = IKprf -- ^ proof implementation
             | IKval -- ^ value implementation

data FunClosure = FCfun
                | FCcloflt -- ^ Flat
                | FCcloptr -- ^ Linear, boxed
                | FCcloref -- ^ Nonlinear, boxed
