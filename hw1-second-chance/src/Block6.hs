{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}

module Block6 where

import           Control.Applicative

newtype Parser s a =
  Parser
    { runParser :: [s] -> Maybe (a, [s])
    }


---------- TASK 1 ----------


-- | `Functor` realisation for `Parser`
instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser parser) =
    let first :: (a -> b) -> (a, c) -> (b, c)
        first f' (a, c) = (f' a, c)
     in Parser (fmap (first f) . parser)

-- | `Applicative` realisation for `Parser`
instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure a = Parser $ \s -> Just (a, s)

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  Parser pf <*> Parser pa = Parser $ \s -> case pf s of
    Nothing -> Nothing
    Just (f, t) -> case pa t of
      Nothing     -> Nothing
      Just (a, r) -> Just (f a, r)

-- | `Monad` realisation for `Parser`
instance Monad (Parser s) where
  return :: a -> Parser s a
  return = pure

  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  Parser p >>= f = Parser $ \s -> case p s of
    Nothing     -> Nothing
    Just (a, r) -> runParser (f a) r

-- | `Alternative` realisation for `Parser`
instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ const Nothing

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  Parser pa <|> Parser pb = Parser $ \s -> case pa s of
    Nothing -> pb s
    smth    -> smth


---------- TASK 2 ----------


-- | Parser, which always succeeds without consuming any input
ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

-- | Parser, which succeeds only on the end of data stream
eof :: Parser s ()
eof = Parser $ \case
  [] -> Just ((), [])
  _  -> Nothing

-- | Function, which takes a Char predicate and constructs a
-- parser which succeeds only if it sees a Char that satisfies the predicate
satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser $ \case
  [] -> Nothing
  x : xs -> if p x then Just (x, xs) else Nothing

-- | Returns the parser, which expects to
-- see exactly a given character and fails otherwise
element :: Eq s => s -> Parser s s
element c = satisfy (== c)

-- | Returns the parser, which expects to
-- see exactly a given string and fails otherwise
stream :: Eq s => [s] -> Parser s [s]
stream = \case
  []     -> pure []
  (x:xs) -> liftA2 (:) (element x) $ stream xs