module FaultyExpressions
  ( Expr (..)
  , evaluate
  , valid1
  , valid2
  , invalid1
  , invalid2
  , invalid3
  ) where

-- Do not modify anything above this line.
--
-- This question is worth 20 POINTS

-- You are given a data type for representing arithmetic and logical expressions.

data Expr
    = Num Int
    | Logical Bool
    | Add Expr Expr
    | Div Expr Expr
    | Less Expr Expr
    | If Expr Expr Expr
  deriving Show

{--
As you may notice, this type allows some meaningless expressions
such as adding Bools or passing an integer instead of a Bool as the first argument to If.

Your task is to implement the function
	evaluate :: Expr -> Maybe Int

If the expression is valid AND its result is integer, `evaluate` returns Just the integer value of the expression. 
If the expression is not valid or its result is Boolean, `evaluate` should return Nothing.

evaluate must follow these rules:

1. Addition is only allowed when both arguments can be evaluated to integers.
2. Division is only allowed when  both arguments can be evaluated to integers AND
   the second argument is not zero. Note: The operation is Integer division
   meaning that the result must also be an integer.
   Its behaviour should be the same as that of Haskell function div.
3. Less is only allowed when both arguments can be evaluated to integers.
   Its result should be True when the first argument is less than the second and False otherwise.
4. If is only allowed when its first argument can be evaluated to Bool
   and the remaining arguments can be evaluated to integers.
   It should return its second argument when the Bool argument is True,
   and return the third argument otherwise.
5. The final result must be an integer. If it’s not, return Nothing.

Note: You are allowed to create helper functions.

Here are some examples of valid expressions:
NOTE: You don't need to modify them. They are given only for your convenience so you can test your implementation.
You can safely ignore them. They will NOT be checked or graded.
--}

-- if 0 < 2 + 3 then 42 else 1 // 0
valid1 :: Expr
valid1 = If (Less (Num 0)
                  (Add (Num 2) (Num 3))
            )
            (Num 42)
            (Div (Num 1) (Num 0))
{--

The result is 42. Note that despite 1 // 0 being an illegal operation,
it is never evaluated and hence the expression is valid.

--}

-- (if False then 1 else 9 // 4) + 5
valid2 :: Expr
valid2 = Add (If (Logical False)
                 (Num 3)
                 (Div (Num 9) (Num 4))
             )
             (Num 5)
{--

The result is 9 // 4 + 5 = 2 + 5 = 7

Here are some examples of invalid expressions:
--}
valid3 = If (Logical False)
            (Num 3)
            (Div (Num 9) (Num 4))
             

-- if 5 < 2 + 3 then 42 else 1 // 0
invalid1 :: Expr
invalid1 = If (Less (Num 5)
                    (Add (Num 2) (Num 3))
              )
              (Num 42)
              (Div (Num 1) (Num 0))

{--
This time, the expression 1 // 0 is evaluated and results in an error.
--}

-- (if 42 then 1 else 9 // 4) + 5
invalid2 :: Expr
invalid2 = Add (If (Num 42)
                   (Num 1)
                   (Div (Num 9) (Num 4))
               )
               (Num 5)
{--
The first argument of If must be evaluated to Boolean but in this example it’s 42
--}

-- (1 + 2 // 1) < 5
invalid3 :: Expr
invalid3 = Less (Add (Num 1)
                     (Div (Num 2) (Num 1))
                )
                (Num 5)

{--
The result is Boolean which is not allowed.

The task.
Implement the following function.
--}

evaluate :: Expr -> Maybe Int
evaluate (Num i) = Just i
evaluate (Logical b) = Nothing

evaluate (Add a b) = sumjust (evaluate a) (evaluate b)
   where sumjust :: Maybe Int -> Maybe Int -> Maybe Int
         sumjust (Just a) (Just b) = Just (a + b)
         sumjust _ _ = Nothing

evaluate (Div a b) = divjust (evaluate a) (evaluate b)
   where divjust :: Maybe Int -> Maybe Int -> Maybe Int
         divjust (Just a) (Just 0) = Nothing
         divjust (Just a) (Just b) =  Just (a `div` b)
         divjust _ _ = Nothing

evaluate (Less a b) = Nothing

evaluate (If (Logical True) a b) = evaluate a
evaluate (If (Logical False) a b) = evaluate b
evaluate (If (Less x y) a b)
   | lessjust (evaluate x) (evaluate y) = evaluate a
   | not $ lessjust (evaluate x) (evaluate y) = evaluate b
   | otherwise = Nothing
   where
      lessjust :: Maybe Int -> Maybe Int -> Bool 
      lessjust (Just a) (Just b) = a < b
      lessjust _ _ = undefined
evaluate (If _ a b) = Nothing
   


