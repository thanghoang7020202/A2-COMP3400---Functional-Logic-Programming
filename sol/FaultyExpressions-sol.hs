module FaultyExpressions
  ( Expr (..)
  , evaluate
  , valid1
  , valid2
  , invalid1
  , invalid2
  , invalid3
  ) where

valid1 :: Expr
valid1 = If (Less (Num 0)
                  (Add (Num 2) (Num 3))
            )
            (Num 42)
            (Div (Num 1) (Num 0))

valid2 :: Expr
valid2 = Add (If (Logical False)
                 (Num 1)
                 (Div (Num 9) (Num 4))
             )
             (Num 5)

invalid1 :: Expr
invalid1 = If (Less (Num 5)
                    (Add (Num 2) (Num 3))
              )
              (Num 42)
              (Div (Num 1) (Num 0))

invalid2 :: Expr
invalid2 = Add (If (Num 42)
                   (Num 1)
                   (Div (Num 9) (Num 4))
               )
               (Num 5)

invalid3 :: Expr
invalid3 = Less (Add (Num 1)
                     (Div (Num 2) (Num 1))
                )
                (Num 5)

data Expr
    = Num Int
    | Logical Bool
    | Add Expr Expr
    | Div Expr Expr
    | Less Expr Expr
    | If Expr Expr Expr
  deriving Show

evaluate :: Expr -> Maybe Int
evaluate (Num x) = Just x
evaluate (Logical _) = Nothing
evaluate (Add expa expb) = (+) <$> (evaluate expa) <*> (evaluate expb)
evaluate (Div expa expb)
   | evaluate expb == Just 0 = Nothing
   | otherwise = div <$> (evaluate expa) <*> (evaluate expb)
evaluate (Less _ _) = Nothing
evaluate (If expa expb expc) = case evalBool expa of
   (Just True)  -> evaluate expb
   (Just False) -> evaluate expc
   Nothing -> Nothing

evalBool :: Expr -> Maybe Bool
evalBool (Logical x) = Just $ x
evalBool (Less expa expb) = (<) <$> (evaluate expa) <*> (evaluate expb)
evalBool _ = Nothing