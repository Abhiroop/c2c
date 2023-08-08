{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module STG where

import Utils

import qualified Data.Map.Strict as M

{- Order of implemention
1. Implementing Lazy Functional Languages on Stock Hardware - SPJ - describes the original STG machine
2. Dynamic Pointer Tagging - https://simonmar.github.io/bib/papers/ptr-tagging.pdf - changes the "tagless" aspect of STG
3. Push/enter vs eval/apply - https://www.microsoft.com/en-us/research/uploads/prod/2016/07/eval-apply-icfp.pdf - changes the notion of function application
4. Not implemented in GHC - Putting the spine back in STG - https://alastairreid.github.io/papers/spine-ifl98.pdf



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

{- Similar projects:

https://github.com/quchen/stgi
https://github.com/bjpop/ministg


-}

type Program = Binds

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

-- | A memory address.
newtype MemAddr = MemAddr Int
    deriving (Show, Eq, Ord)

-- | The possible values of the STG machine
--   These tags are not present in actual implementations.
data Value = Addr MemAddr | PrimInt Integer deriving (Show, Eq)


-- | The STG representation of a closure
--   (vs \π xs -> e) ws
--   where ws gives the value of each free variable
data Closure = Closure LambdaForm [Value] deriving (Show, Eq)

data STGState =
     STGState { stgCode        :: Code
              , stgArgStack    :: Stack Value -- argument stack
              , stgReturnStack :: Stack Continuation -- return stack
              , stgUpdateStack :: Stack UpdateFrame  -- update stack
              , stgHeap        :: Heap
              , stgGlobalEnv   :: Globals
              }
              deriving (Show, Eq)

-- | STG Code state
data Code =
    -- | Evaluate an expression in a local environment and apply
    --   its value to the arguments on the argument stack
    Eval Expr Locals

    -- | Apply the closure at `MemAddr` to the arguments on argument stack
  | Enter MemAddr

    -- | Return the constructor applied to the values to the continuation
    --   on the return stack
  | ReturnCon Constr [Value]

    -- | Return the primitive integer to the continuation on the return stack
  | ReturnInt Integer
  deriving (Show, Eq)

-- | The STG Heap
type Heap = M.Map MemAddr Closure

-- | STG globals
--   mapping from variables at the top level to the address of
--   top-level closures; Note that the top-level closures is updatable
--   The address of the globals should not change during execution
type Globals = M.Map Var Value

data Continuation -- not yet defined
  =  Cont deriving (Show, Eq)

data UpdateFrame  -- not yet defined
  =  UpdateFrame deriving (Show, Eq)

-- | Local environment
--   maps local definitions to their respective values
type Locals = M.Map Var Value

val :: Locals -> Globals -> Atom -> Maybe Value
val locals globals (Lit literal) =
  pure $ PrimInt literal
val locals globals (Var var) = do
  case (M.lookup var locals) of
    Just v  -> Just v
    Nothing ->
      case (M.lookup var globals) of
        Just v_ -> Just v_
        Nothing -> Nothing

initSTGState :: Program -> STGState
initSTGState program =
  case M.lookup "main" program of
    Nothing -> error "STGErr : Missing main function!"
    Just (LambdaForm _ updflag _ mainExpr) ->
      STGState { stgCode        = code
               , stgArgStack    = initStack
               , stgReturnStack = initStack
               , stgUpdateStack = initStack
               , stgHeap        = heap
               , stgGlobalEnv   = globals
               }
      where
        {- Construct the CODE component -}
        code = Eval mainExpr M.empty
        bindsList = M.toList program
        {- Allocate closure on heap without pointing
           to the free variables that are globals -}
        heapList = zip (map MemAddr [0..])
                   (map ((\lf -> Closure lf []) . snd) bindsList)
        {- Construct the global environment using the heapList above -}
        heapAddrs = map (Addr . fst) heapList :: [Value]
        globals   = M.fromList $ zip (map fst bindsList) heapAddrs
        {- Mutate the heap to point to location of the free variables
           that occur as globals and this is the final heap -}
        heap = M.fromList $ map updClosure heapList
        updClosure :: (MemAddr, Closure) -> (MemAddr, Closure)
        updClosure (maddr, (Closure lf@(LambdaForm freevars _ _ _) _))
          = (maddr, cls')
          where
            cls' = Closure lf freevars'
            freevars' = map (\fv -> case M.lookup fv globals of
                                      Just v -> v
                                      Nothing ->
                                        error "STGErr : Unbound free variable \
                                               \while constructing initial STG \
                                               \state"
                            ) freevars

--------------------- STATE TRANSITION RULES --------------------

rule_1_funcApp :: STGState -> Maybe STGState
rule_1_funcApp s@(STGState { stgCode = Eval (AppF f xs) locals
                           , stgArgStack  = argStack
                           , stgGlobalEnv = globals
                           }) = do
  (Addr a) <- val locals globals (Var f)
  args     <- traverse (val locals globals) xs
  Just $ s { stgCode = Enter a
           , stgArgStack = args >>: argStack
           }
rule_1_funcApp _ = Nothing

rule_2_enterNonUpdatable :: STGState -> Maybe STGState
rule_2_enterNonUpdatable s@(STGState { stgCode = Enter a
                                     , stgArgStack = argStack
                                     , stgHeap = heap
                                     }) = do
  Closure (LambdaForm vs NotUpdatable xs e) ws_free <- M.lookup a heap
  let freeLocals  = zip vs ws_free
  let boundLocals = zip xs argStack -- zip chooses the smallest list; according to the semantics
                                    -- length (argStack) >= length xs so this should be fine
  let locals = M.fromList $ freeLocals ++ boundLocals
  let as' = drop (length xs) argStack
  Just $ s { stgCode = Eval e locals
           , stgArgStack = as'
           }
rule_2_enterNonUpdatable _ = Nothing

-- The only difference between `let` and `letrec` is in which
-- environment the variables are looked up; `locals` for `let`
-- and `locals'` for `letrec` to account for the mutual recursion
rule_3_let :: STGState -> Maybe STGState
rule_3_let s@(STGState { stgCode = Eval (Let binds e) locals
                       , stgHeap = heap
                       }) = do
  let bindslist = M.toList binds
  let boundVars = map fst bindslist
  let (MemAddr maxaddr) = fst (M.findMax heap)
  let addrs = map MemAddr [(maxaddr + 1) .. (maxaddr + 1 + length bindslist)]
  let locals' = insertMany (zip boundVars (map Addr addrs)) locals
  let lambdaforms = map snd bindslist
  newClosures <- traverse (\lf@(LambdaForm vs _ _ _) -> do
                              freevarvals <- traverse (flip M.lookup locals) vs
                              Just $ Closure lf freevarvals) lambdaforms
  let heap' = insertMany (zip addrs newClosures) heap
  Just $ s { stgCode = Eval e locals'
           , stgHeap = heap'
           }
rule_3_let s@(STGState { stgCode = Eval (LetRec binds e) locals
                       , stgHeap = heap
                       }) = do
  let bindslist = M.toList binds
  let boundVars = map fst bindslist
  let (MemAddr maxaddr) = fst (M.findMax heap)
  let addrs = map MemAddr [(maxaddr + 1) .. (maxaddr + 1 + length bindslist)]
  let locals' = insertMany (zip boundVars (map Addr addrs)) locals
  let lambdaforms = map snd bindslist
  newClosures <- traverse (\lf@(LambdaForm vs _ _ _) -> do
                              freevarvals <- traverse (flip M.lookup locals') vs
                              Just $ Closure lf freevarvals) lambdaforms
  let heap' = insertMany (zip addrs newClosures) heap
  Just $ s { stgCode = Eval e locals'
           , stgHeap = heap'
           }
rule_3_let _ = Nothing
