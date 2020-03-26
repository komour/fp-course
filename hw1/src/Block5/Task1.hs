module Block5.Task1 where

-- | Data type for binary operations
data Operation = Sum
    | Sub
    | Mul
    | Div
    | Pow
    deriving (Show)

-- | Data type for expressions
data Expr = Const Int
    | BinaryOp
    { op :: Operation
    , l  :: Expr
    , r  :: Expr
    }
    deriving (Show)

-- | Data type for arithmetic errors
data ArithmeticError = DivideByZero
    | PowNegate
    deriving (Show, Eq)

-- | Takes an expression and returns it's result or an error
eval :: Expr -> Either ArithmeticError Int
eval (Const x) = Right x
eval (BinaryOp operation left right) =
  eval right >>= core (eval left)
  where
    core :: Either ArithmeticError Int -> Int -> Either ArithmeticError Int
    core e x = case operation of
      Sum -> fmap (+ x) e
      Sub -> fmap (\y -> y - x) e
      Mul -> fmap (* x) e
      Div -> if x == 0 then Left DivideByZero else fmap (`div` x) e
      Pow -> if x < 0 then Left PowNegate else fmap (  ^ x) e
