import Control.Monad (liftM, ap)

data Parser a = P (String -> [(a, String)])

instance Functor Parser where
  fmap = liftM
instance Applicative Parser where
  pure = return
  (<*>) = ap
instance Monad Parser where
  -- return :: a -> Parser a
  return v = P $ \inp -> [(v, inp)]
  p >>= f = P $ \inp -> concat [parse (f v) inp' | (v, inp') <- parse p inp]

item :: Parser Char
item = P $ \inp -> case inp of
                [] -> []
                (x:xs) -> [(x, xs)]

failure :: Parser a
failure = P $ \_ -> []

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P $ \inp -> case parse p inp of
                    [] -> parse q inp
                    [(v, s)] -> [(v, s)]

seq :: Parser a -> Parser b -> Parser (a, b)
p `seq` q = p >>= \x ->
            q >>= \y ->
            return (x, y)

seq' :: Parser a -> Parser b -> Parser (a, b)
p `seq'` q = do { x <- p
                ; y <- q
                ; return (x, y) }

-- first_third :: Parser (Char, Char)
-- first_third = do x <- item
--                  item
--                  y <- item
--                  returnP (x, y)