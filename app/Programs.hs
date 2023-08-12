module Programs where

import STG
import qualified Data.Map.Strict as M

test :: Program -> STGState
test = eval . initSTGState

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


prog10 :: Program
prog10 = M.fromList [ ("map", map_lf)
                   , ("list", list_lf)
                   , ("plus_one", plus_one_lf)
                   , ("main", mainExpr)
                   ]
  where
    map_lf = LambdaForm [] NotUpdatable ["f", "xs"] case_expr
    case_expr = Case (AppF "xs" []) (AlgAlts [alt1, alt2] Nothing)
    alt1 = AlgAlt nil_constr [] (AppC nil_constr [])
    alt2 = AlgAlt cons_constr ["y", "ys"] let_expr
    let_expr = Let let_binds (AppC cons_constr [Var "fy", Var "mfy"])
    let_binds = M.insert "mfy" lf2 (M.insert "fy" lf1 M.empty)
    lf1 = LambdaForm ["f", "y"] Updatable [] (AppF "f" [Var "y"])
    lf2 = LambdaForm ["f", "ys"] Updatable [] (AppF "map" [Var "f", Var "ys"])
    nil_constr  = Constr "Nil"
    cons_constr = Constr "Cons"

    list_lf = LambdaForm [] Updatable [] let_expr_2
    let2_binds = M.fromList [ ("one", one_lf)
                            , ("two", two_lf)
                            , ("nil", nil_lf)
                            , ("temp", temp_lf)
                            ]
    one_lf = LambdaForm [] Updatable [] (LitE 1)
    two_lf = LambdaForm [] Updatable [] (LitE 2)
    nil_lf = LambdaForm [] Updatable [] (AppC nil_constr [])
    temp_lf = LambdaForm [] Updatable [] (AppC cons_constr [Var "two", Var "nil"])
    let_expr_2 = Let let2_binds (AppC cons_constr [Var "one", Var "temp"])


    plus_one_lf = LambdaForm [] NotUpdatable ["x"] (AppP PlusOp (Var "x") (Lit 1))
    -- map and list are global constants
    mainExpr = LambdaForm [] Updatable [] (AppF "map" [Var "plus_one", Var "list"])

testSuite :: IO ()
testSuite
  | result = putStrLn "All tests passed"
  | otherwise = putStrLn "Some failures"
  where
    result =
      foldr (&&) True
      [stgCode (test p) == res
      | (p, res) <- [ (prog1, ReturnInt 5)
                    , (prog2, ReturnInt 25)
                    , (prog3, ReturnInt 55)
                    ]]
