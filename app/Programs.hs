module Programs where

import STG
import qualified Data.Map.Strict as M

test :: Program -> STGState
test = eval . initSTGState

testDebug :: Program -> IO STGState
testDebug =  evalDebug . initSTGState

{- main = 2 + 3 -}
prog1 :: Program
prog1 = M.fromList [("main", main_lf)]
  where
    main_lf = LambdaForm [] Updatable [] (AppP PlusOp (Lit 2) (Lit 3))


{- main = let x = 2 + 3
           in (case x of
                    y -> y)
-}
prog2 :: Program
prog2 = M.fromList [("main", main_lf)]
  where
    main_lf = LambdaForm [] Updatable [] (Let let_binds let_expr)
    let_binds =
      M.fromList [("x", LambdaForm [] Updatable [] (AppP PlusOp (Lit 10) (Lit 15)))]
    let_expr  = Case (AppF "x" []) case_alts
    case_alts :: Alts
    case_alts = AlgAlts [] (Just (DefaultBound "y" (AppF "y" [])))

{- main = let x = 55
           in (case x of
                    y -> y)
-}
prog3 :: Program
prog3 = M.fromList [("main", main_lf)]
  where
    main_lf = LambdaForm [] Updatable [] (Let let_binds let_expr)
    let_binds =
      M.fromList [("x", LambdaForm [] NotUpdatable [] (LitE 55))]
    let_expr  = Case (AppF "x" []) case_alts
    case_alts :: Alts
    case_alts = AlgAlts [] (Just (DefaultBound "y" (AppF "y" [])))

{- main = let f x = x + 5
           in f 3
-}
prog4 :: Program
prog4 = M.fromList [("main", main_lf)]
  where
    main_lf   = LambdaForm [] Updatable [] (Let let_binds let_expr)
    let_binds =
      M.fromList [("f", LambdaForm [] NotUpdatable ["x"]
                        (AppP PlusOp (Var "x") (Lit 5)))]
    let_expr  = AppF "f" [Lit 3]

{-  main = [1, 2]

    ==

    main = letrec nil  = Nil
                   l1  = Cons 2 nil
               in Cons 1 l1
-}
prog5 :: Program
prog5 = M.fromList [("main", main_lf)]
  where
    main_lf = LambdaForm [] Updatable [] (LetRec let_binds let_expr)
    let_binds =
      M.fromList [ ("nil", LambdaForm [] NotUpdatable []
                           (AppC (Constr "Nil") []))
                 , ("l1" , LambdaForm ["nil"] NotUpdatable []
                           (AppC (Constr "Cons") [Lit 2, Var "nil"]))
                 ]
    let_expr  = (AppC (Constr "Cons") [Lit 1, Var "l1"])

{-  list = [1, 2]

    head [] = []
    head (x:_) = x

    main = head list

    ==

    list = letrec nil  = Nil
                   l1  = Cons 2 nil
               in Cons 1 l1

     head = \l -> case l of
                      Nil -> Nil
                      Cons x xs -> x

     main = head list
-}
prog6 :: Program
prog6 = M.fromList [ ("list", list_lf)
                   , ("head", head_lf)
                   , ("main", main_lf)
                   ]
  where
    list_lf = LambdaForm [] Updatable [] (LetRec let_binds let_expr)
    let_binds =
      M.fromList [ ("nil", LambdaForm [] NotUpdatable []
                           (AppC (Constr "Nil") []))
                 , ("l1" , LambdaForm ["nil"] NotUpdatable []
                           (AppC (Constr "Cons") [Lit 2, Var "nil"]))
                 ]
    let_expr  = (AppC (Constr "Cons") [Lit 1, Var "l1"])

    head_lf = LambdaForm [] NotUpdatable ["l"] case_expr
    case_expr = Case (AppF "l" []) (AlgAlts [alt1, alt2] Nothing)
    alt1 = AlgAlt (Constr "Nil") [] (AppC (Constr "Nil") [])
    alt2 = AlgAlt (Constr "Cons") ["x", "xs"] (AppF "x" [])

    -- head and list are global constants
    main_lf = LambdaForm [] Updatable [] (AppF "head" [Var "list"])

{-  list = [1, 2]

    main = letrec sumrec = \l -> case l of
                                   Nil -> 0
                                   Cons x xs ->
                                     case (sumrec xs) of
                                         y -> x + y
               in sumrec list

-}
prog7 :: Program
prog7 = M.fromList [ ("list", list_lf)
                   , ("main", main_lf)
                   ]
  where
    list_lf = LambdaForm [] Updatable [] (LetRec let_binds let_expr)
    let_binds =
      M.fromList [ ("nil", LambdaForm [] NotUpdatable []
                           (AppC (Constr "Nil") []))
                 , ("l1" , LambdaForm ["nil"] NotUpdatable []
                           (AppC (Constr "Cons") [Lit 2, Var "nil"]))
                 ]
    let_expr  = (AppC (Constr "Cons") [Lit 1, Var "l1"])

    sum_lf = LambdaForm ["sumrec"] NotUpdatable ["l"] case_expr
    case_expr = Case (AppF "l" []) (AlgAlts [alt1, alt2] Nothing)
    alt1 = AlgAlt (Constr "Nil") [] (LitE 0)
    alt2 =
      AlgAlt (Constr "Cons") ["x", "xs"]
      (Case (AppF "sumrec" [Var "xs"])
        (PrimAlts [] (Just (DefaultBound "y" (AppP PlusOp (Var "x") (Var "y"))))))

    -- list is a global constant
    main_lf =
      LambdaForm [] Updatable []
      (LetRec (M.fromList [("sumrec", sum_lf)]) (AppF "sumrec" [Var "list"]))

{-  list = [1, 2]

    sum [] = 0
    sum (x:xs) = x + sum xs

    main = sum list

    ==

    list = letrec nil  = Nil
                   l1  = Cons 2 nil
               in Cons 1 l1

     sum = \l -> case l of
                      Nil -> 0
                      Cons x xs ->
                        case (sum xs) of
                          y -> (+ x y)

     main = sum list
-}
prog8 :: Program
prog8 = M.fromList [ ("list", list_lf)
                   , ("sum" , sum_lf)
                   , ("main", main_lf)
                   ]
  where
    list_lf = LambdaForm [] Updatable [] (LetRec let_binds let_expr)
    let_binds =
      M.fromList [ ("nil", LambdaForm [] NotUpdatable []
                           (AppC (Constr "Nil") []))
                 , ("l1" , LambdaForm ["nil"] NotUpdatable []
                           (AppC (Constr "Cons") [Lit 2, Var "nil"]))
                 ]
    let_expr  = (AppC (Constr "Cons") [Lit 1, Var "l1"])

    sum_lf = LambdaForm [] NotUpdatable ["l"] case_expr
    case_expr = Case (AppF "l" []) (AlgAlts [alt1, alt2] Nothing)
    alt1 = AlgAlt (Constr "Nil") [] (LitE 0)
    alt2 =
      AlgAlt (Constr "Cons") ["x", "xs"]
      (Case (AppF "sum" [Var "xs"])
        (PrimAlts [] (Just (DefaultBound "y" (AppP PlusOp (Var "x") (Var "y"))))))

    -- sum and list are global constants
    main_lf =
      LambdaForm [] Updatable []
      (AppF "sum" [Var "list"])

{-  list = [1, 2]

    sum [] res = res
    sum (x:xs) res = sum xs (res + x)

    main = sum list 0

    ==

    list = letrec nil  = Nil
                   l1  = Cons 2 nil
               in Cons 1 l1

     sum = \l res -> case l of
                         Nil -> res
                         Cons x xs ->
                            case (res + x) of
                               y -> sum xs y

     main = sum list 0
-}
prog9 :: Program
prog9 = M.fromList [ ("list", list_lf)
                   , ("sum" , sum_lf)
                   , ("main", main_lf)
                   ]
  where
    list_lf = LambdaForm [] Updatable [] (LetRec let_binds let_expr)
    let_binds =
      M.fromList [ ("nil", LambdaForm [] NotUpdatable []
                           (AppC (Constr "Nil") []))
                 , ("l1" , LambdaForm ["nil"] NotUpdatable []
                           (AppC (Constr "Cons") [Lit 2, Var "nil"]))
                 ]
    let_expr  = (AppC (Constr "Cons") [Lit 1, Var "l1"])

    sum_lf = LambdaForm [] NotUpdatable ["l", "res"] case_expr
    case_expr = Case (AppF "l" []) (AlgAlts [alt1, alt2] Nothing)
    alt1 = AlgAlt (Constr "Nil") [] (AppF "res" [])
    alt2 =
      AlgAlt (Constr "Cons") ["x", "xs"]
      (Case (AppP PlusOp (Var "res") (Var "x"))
        (PrimAlts [] (Just (DefaultBound "y" (AppF "sum" [(Var "xs"), (Var "y")])))))

    main_lf =
      LambdaForm [] Updatable []
      (AppF "sum" [Var "list", Lit 0])


{-  list = [1, 2]


    plus_one x = x + 1

    map f [] = []
    map f (x:xs) = f x : (map f xs)

    sum [] res = res
    sum (x:xs) res = sum xs (res + x)

    main = sum (map plus_one list)

    ==

    list = letrec nil  = Nil
                   l1  = Cons 2 nil
               in Cons 1 l1

    plus_one = \x -> x + 1

    map = \f l -> case l of
                    Nil -> Nil
                    Cons x xs ->
                       case (f x) of
                         y -> case (map f xs) of
                                r -> y : r

    sum = \l res -> case l of
                        Nil -> res
                        Cons x xs ->
                           case (res + x) of
                              y -> sum xs y

    main = case (map plus_one list) of
                 z -> sum z


-}
prog10 :: Program
prog10 = M.fromList [ ("list", list_lf)
                    , ("plus_one", plus_one_lf)
                    , ("map", map_lf)
                    , ("sum", sum_lf)
                    , ("main", mainExpr)
                    ]
  where
    list_lf = LambdaForm [] Updatable [] (LetRec let_binds let_expr)
    let_binds =
      M.fromList [ ("nil", LambdaForm [] NotUpdatable []
                           (AppC (Constr "Nil") []))
                 , ("l1" , LambdaForm ["nil"] NotUpdatable []
                           (AppC (Constr "Cons") [Lit 2, Var "nil"]))
                 ]
    let_expr  = (AppC (Constr "Cons") [Lit 1, Var "l1"])

    plus_one_lf =
      LambdaForm [] NotUpdatable ["x"] (AppP PlusOp (Var "x") (Lit 1))

    map_lf = LambdaForm [] NotUpdatable ["f", "l"] case_expr
    case_expr = Case (AppF "l" []) (AlgAlts [alt1, alt2] Nothing)
    alt1 = AlgAlt nil_constr [] (AppC nil_constr [])
    alt2 =
      AlgAlt cons_constr ["x", "xs"]
       (Case (AppF "f" [Var "x"])
        (PrimAlts []
         (Just
          (DefaultBound "y"
           (Case (AppF "map" [Var "f", Var "xs"])
            (AlgAlts []
             (Just
              (DefaultBound "r"
               (AppC cons_constr [Var "y", Var "r"])
              )
             )
            )
           )
          )
         )
        )
       )
    nil_constr  = Constr "Nil"
    cons_constr = Constr "Cons"

    sum_lf = LambdaForm [] NotUpdatable ["l", "res"] case_sum_expr
    case_sum_expr = Case (AppF "l" []) (AlgAlts [alts1, alts2] Nothing)
    alts1 = AlgAlt (Constr "Nil") [] (AppF "res" [])
    alts2 =
      AlgAlt (Constr "Cons") ["x", "xs"]
      (Case (AppP PlusOp (Var "res") (Var "x"))
        (PrimAlts []
         (Just
          (DefaultBound "y" (AppF "sum" [(Var "xs"), (Var "y")])))))

    -- map and list are global constants
    mainExpr = LambdaForm [] Updatable []
      (Case (AppF "map" [Var "plus_one", Var "list"])
       (AlgAlts []
        (Just (DefaultBound "z" (AppF "sum" [Var "z", Lit 0])))))


testSuite :: IO ()
testSuite
  | result    = putStrLn "All tests passed"
  | otherwise = putStrLn "Some failures"
  where
    result =
      foldr (&&) True
      [stgCode (test p) == res
      | (p, res) <- [ (prog1, ReturnInt 5)
                    , (prog2, ReturnInt 25)
                    , (prog3, ReturnInt 55)
                    , (prog4, ReturnInt 8)
                    -- prog5 returns a WHNF list
                    , (prog6, ReturnInt 1)
                    , (prog7, ReturnInt 3)
                    , (prog8, ReturnInt 3)
                    , (prog9, ReturnInt 3)
                    , (prog10, ReturnInt 5)
                    ]]
