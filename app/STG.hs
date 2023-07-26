module STG where

import qualified Data.Map as M

{- Order of implemention
1. Implementing Lazy Functional Languages on Stock Hardware - SPJ - describes the original STG machine
2. Dynamic Pointer Tagging - https://simonmar.github.io/bib/papers/ptr-tagging.pdf - changes the "tagless" aspect of STG
3. Push/enter vs eval/apply - https://www.microsoft.com/en-us/research/uploads/prod/2016/07/eval-apply-icfp.pdf - changes the notion of function application

The first paper does

STG --> C

GHC does

STG --> C-- --> x86

How important is the C-- translation?
How about compiling from STG to CHERI-C?

STG --> CHERI-C --> CHERI
STG --> LLVM    --> CHERI
STG             --> CHERI

-}



{- STG features

1. All function and constructor arguments are simple variables or constants.
2. All constructor and built-in functions are saturated.
3. Pattern matching is performed only by case expressions.
4. There is a special form of binding
   f = {v1,...vn} \pi {x1,....,xm} -> e
   where {v1,...vn} are the free variables and pi indicates whether
   closure is updatable
5. STG supports unboxed values

-}

type Prog = Binds

type Var = String -- "var1, var2,...,varn"

type FreeVar  = Var

type BoundVar = Var

type Literal = Integer -- these are unboxed

data Atom = Var Var | Lit Literal deriving (Show, Eq)

type Binds = M.Map Var LambdaForm

data LambdaForm =
  LambdaForm [FreeVar] UpdateFlag [BoundVar] Expr
  deriving (Show, Eq)

data UpdateFlag = Updatable
                | NotUpdatable
                deriving (Show, Eq)

data PrimOp = PlusOp
            | MinusOp
            | MulOp
            | DivOp
            deriving (Show, Eq)

data Expr = Let    Binds  Expr
          | LetRec Binds  Expr
          | AppF   Var    [Atom]
          | AppC   Constr [Atom]
          | AppP   PrimOp [Atom]
          | Case   Expr  Alts
          | LitE   Literal
          deriving (Show, Eq)

data Alts = AlgAlts  [AlgAlt]  DefaultAlt
          | PrimAlts [PrimAlt] DefaultAlt
          deriving (Show, Eq)

data AlgAlt = AlgAlt Constr [Var] Expr deriving (Show, Eq)

data PrimAlt = PrimAlt Literal Expr deriving (Show, Eq)

data DefaultAlt
  = DefaultBound Var Expr | DefaultNotBound Expr
  deriving (Show, Eq)

newtype Constr = Constr String
  deriving (Show, Eq)

{-

Operational Reading

Func app  <==> Tail call
Let expr  <==> Heap allocation
Case expr <==> Evaluation
Constructor application <==> Return to continuation

-}
