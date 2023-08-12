{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module STG where

import Data.Foldable (asum)
import Utils

import qualified Data.List as L
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
          | AppP   PrimOp Atom Atom
          | Case   Expr  Alts
          | LitE   Literal
          deriving (Show, Eq)

data Alts = AlgAlts  [AlgAlt]  (Maybe DefaultAlt)
          | PrimAlts [PrimAlt] (Maybe DefaultAlt)
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

type Continuation = (Alts, Locals)

type UpdateFrame = (Stack Value, Stack Continuation, MemAddr)

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

malloc :: Heap -> MemAddr
malloc h = MemAddr (maddr + 1)
  where
    (MemAddr maddr) = fst (M.findMax h)

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
  let (MemAddr newaddr) = malloc heap
  let addrs = map MemAddr [newaddr .. (newaddr + length bindslist)]
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
  let (MemAddr newaddr) = malloc heap
  let addrs = map MemAddr [newaddr .. (newaddr + length bindslist)]
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

rule_4_case :: STGState -> Maybe STGState
rule_4_case s@(STGState { stgCode = Eval (Case e alts) locals
                        , stgReturnStack = rs
                        }) =
  Just $ s { stgCode = Eval e locals
           , stgReturnStack = (alts, locals) >: rs
           }
rule_4_case _ = Nothing

rule_5_constructorApp :: STGState -> Maybe STGState
rule_5_constructorApp s@(STGState { stgCode = Eval (AppC c xs) locals
                                  , stgGlobalEnv = globals
                                  }) = do
  constr_args <- traverse (val locals globals) xs
  Just $ s { stgCode = ReturnCon c constr_args }
rule_5_constructorApp _ = Nothing


-- This function handles rules 6, 7 and 8 as three case branches
rule_6_7_8_algebraicMatch :: STGState -> Maybe STGState
rule_6_7_8_algebraicMatch s@(STGState { stgCode = ReturnCon c ws
                                      , stgReturnStack = (alts,locals):rs
                                      , stgHeap = heap
                                      }) = do
  alg_or_def_alt <- lookupAlgebraicAlt alts c
  case alg_or_def_alt of
    {- RULE 6 -}
    Right (AlgAlt _ vs e) -> do
      let locals' = insertMany (zip vs ws) locals
      Just $ s { stgCode = Eval e locals'
               , stgReturnStack = rs
               }
    {- RULE 7 -}
    Left (DefaultNotBound e_d) ->
      Just $ s { stgCode = Eval e_d locals
               , stgReturnStack = rs
               }
    {- RULE 8 -}
    Left (DefaultBound v e_d) -> do
      let maddr = malloc heap
      let locals' = M.insert v (Addr maddr) locals
      let vs = map (\i -> "rule_8_" <> show i) (take (length ws) [1..])
      let clos = Closure (LambdaForm vs NotUpdatable [] (AppC c (map Var vs))) ws
      let heap' = M.insert maddr clos heap
      Just $ s { stgCode = Eval e_d locals'
               , stgReturnStack = rs
               , stgHeap = heap'
               }
rule_6_7_8_algebraicMatch _ = Nothing

lookupAlgebraicAlt :: Alts -> Constr -> Maybe (Either DefaultAlt AlgAlt)
lookupAlgebraicAlt (AlgAlts alts def) c =
  case (L.find (\(AlgAlt constr _ _) -> constr == c) alts) of
    Just a  -> Just (Right a)
    Nothing -> case def of
      Nothing -> Nothing
      Just d' -> Just (Left d')
lookupAlgebraicAlt (PrimAlts _ _) _ = Nothing

rule_9_primitiveLiteralEval :: STGState -> Maybe STGState
rule_9_primitiveLiteralEval s@(STGState { stgCode = Eval (LitE k) locals }) = do
  Just $ s { stgCode = ReturnInt k }
rule_9_primitiveLiteralEval _ = Nothing

rule_10_primitiveLiteralApp :: STGState -> Maybe STGState
rule_10_primitiveLiteralApp s@(STGState { stgCode = Eval (AppF f []) locals }) = do
  (PrimInt k) <- M.lookup f locals
  Just $ s { stgCode = ReturnInt k }
rule_10_primitiveLiteralApp _ = Nothing

-- This function handles rules 11, 12 and 13 as three case branches
rule_11_12_13_primitiveMatch :: STGState -> Maybe STGState
rule_11_12_13_primitiveMatch s@(STGState { stgCode = ReturnInt k
                                         , stgReturnStack = (alts, locals):rs
                                         }) = do
  prim_or_def_alt <- lookupPrimitiveAlt alts k
  case prim_or_def_alt of
    {- RULE 11 -}
    Right (PrimAlt _ e) ->
      Just $ s { stgCode = Eval e locals
               , stgReturnStack = rs
               }
    {- RULE 12 -}
    Left (DefaultBound x e) -> do
      let locals' = M.insert x (PrimInt k) locals
      Just $ s { stgCode = Eval e locals'
               , stgReturnStack = rs
               }
    {- RULE 13 -}
    Left (DefaultNotBound e) ->
      Just $ s { stgCode = Eval e locals
               , stgReturnStack = rs
               }

rule_11_12_13_primitiveMatch _ = Nothing

lookupPrimitiveAlt :: Alts -> Literal -> Maybe (Either DefaultAlt PrimAlt)
lookupPrimitiveAlt (PrimAlts alts def) k =
  case (L.find (\(PrimAlt l _) -> l == k) alts) of
    Just a  -> Just (Right a)
    Nothing -> case def of
      Nothing -> Nothing
      Just d' -> Just (Left d')
lookupPrimitiveAlt (AlgAlts _ _) _ = Nothing

-- extended rule 14 that can handle all four combinations of
-- var `op` var
-- lit `op` lit
-- var `op` lit
-- lit `op` var
rule_14_primop :: STGState -> Maybe STGState
rule_14_primop s@(STGState { stgCode = Eval (AppP op x1 x2) locals }) = do
  (PrimInt i1) <- case x1 of
                    Var v1  -> M.lookup v1 locals
                    Lit l1 -> Just (PrimInt l1)
  (PrimInt i2) <- case x2 of
                    Var v2  -> M.lookup v2 locals
                    Lit l2 -> Just (PrimInt l2)
  let res = case op of
              PlusOp  -> i1 + i2
              MinusOp -> i1 - i2
              MulOp   -> i1 - i2
              DivOp   -> i1 `div` i2
  Just $ s { stgCode = ReturnInt res }
rule_14_primop _ = Nothing

rule_15_enterUpdatable :: STGState -> Maybe STGState
rule_15_enterUpdatable s@(STGState { stgCode = Enter a
                                   , stgArgStack = argStack
                                   , stgReturnStack = returnStack
                                   , stgUpdateStack = updStack
                                   , stgHeap = heap
                                   }) = do
  Closure (LambdaForm vs Updatable [] e) ws_free <- M.lookup a heap
  let locals = M.fromList $ zip vs ws_free
  Just $ s { stgCode        = Eval e locals
           , stgArgStack    = initStack
           , stgReturnStack = initStack
           , stgUpdateStack = (argStack, returnStack, a) >: updStack
           }
rule_15_enterUpdatable _ = Nothing

-- Contrast this with rule_6_7_8 where the return stack is not empty
-- Now upon finding the empty return stack it sets the update flag
rule_16_missingReturnUpdate :: STGState -> Maybe STGState
rule_16_missingReturnUpdate s@(STGState { stgCode = ReturnCon c ws
                                        , stgArgStack = []
                                        , stgReturnStack = []
                                        , stgUpdateStack = (as_u, rs_u, a_u):updStack
                                        , stgHeap = heap
                                   }) = do
  let vs = map (\i -> "rule_16_" <> show i) (take (length ws) [1..])
  let clos = Closure (LambdaForm vs NotUpdatable [] (AppC c (map Var vs))) ws
  let heap' = M.insert a_u clos heap -- mutating the heap (a_u already present)
  Just $ s { stgCode = ReturnCon c ws
           , stgArgStack = as_u
           , stgReturnStack = rs_u
           , stgUpdateStack = updStack
           , stgHeap = heap'
           }
rule_16_missingReturnUpdate _ = Nothing

rule_17a_missingArgUpdate :: STGState -> Maybe STGState
rule_17a_missingArgUpdate s@(STGState { stgCode = Enter a
                                      , stgArgStack = argStack
                                      , stgReturnStack = []
                                      , stgUpdateStack = (as_u, rs_u, a_u):updStack
                                      , stgHeap = heap
                                      }) = do
  (Closure (LambdaForm vs NotUpdatable xs e) ws_free) <- M.lookup a heap
  if (length argStack < length xs)
  then do
    let xs_1 = take (length argStack) xs
    -- let xs_2 = drop (length argStack) xs
    -- XXX: using the length of the heap to simulate randomness
    let f = "rule_17a_" <> show (length (M.toList heap))
    let clos = Closure (LambdaForm (f:xs_1) NotUpdatable []
                         (AppF f (map Var xs_1))) ((Addr a):argStack)
    let heap' = M.insert a_u clos heap
    Just $ s { stgCode = Enter a
             , stgArgStack = argStack ++ as_u
             , stgReturnStack = rs_u
             , stgUpdateStack = updStack
             , stgHeap = heap'
             }
  else Nothing
rule_17a_missingArgUpdate _ = Nothing

rules :: [STGState -> Maybe STGState]
rules = [ rule_1_funcApp
        , rule_2_enterNonUpdatable
        , rule_3_let
        , rule_4_case
        , rule_5_constructorApp
        , rule_6_7_8_algebraicMatch
        , rule_9_primitiveLiteralEval
        , rule_10_primitiveLiteralApp
        , rule_11_12_13_primitiveMatch
        , rule_14_primop
        , rule_15_enterUpdatable
        , rule_16_missingReturnUpdate
        , rule_17a_missingArgUpdate
        ]

-- | Main evaluator
-- Continue evaluating till no matching rules are
-- found and then return the final state
eval :: STGState -> STGState
eval state =
  case asum [ rule state | rule <- rules] of
    Nothing -> state
    Just state' -> eval state'
