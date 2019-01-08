import Control.Monad
import Control.Applicative

data Parser a = P (String -> [(a, String)])

instance Functor Parser where
  fmap = liftM
instance Applicative Parser where
  pure = result
  (<*>) = ap
instance Monad Parser where
  return = pure
  (>>=) = bind
instance Alternative Parser where
  empty = failure
  (<|>) = choice
instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

result :: a -> Parser a
result v = P $ \inp -> [(v, inp)]

bind :: Parser a -> (a -> Parser b) -> Parser b
p `bind` f = P $ \inp -> concat [parse (f v) inp' | (v, inp') <- parse p inp]

failure :: Parser a
failure = P $ \_ -> []

choice :: Parser a -> Parser a -> Parser a
p `choice` q = P $ \inp -> parse p inp ++ parse q inp

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P $ \inp -> case parse (p <|> q) inp of
                        [] -> []
                        (x:xs) -> [x]

seq :: Parser a -> Parser b -> Parser (a, b)
p `seq` q = do { x <- p
               ; y <- q
               ; return (x, y) }

item :: Parser Char
item = P $ \inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x, xs)]

sat :: Parser a -> (a -> Bool) -> Parser a
sat parser predicate = do { x <- parser
                          ; if predicate x 
                            then return x 
                            else mzero
                          }

satC :: (Char -> Bool) -> Parser Char
satC = sat item

char :: Char -> Parser Char
char c = satC (c ==)

digit :: Parser Char
digit = satC (\d -> d >= '0' && d <= '9')

lower :: Parser Char
lower = satC (\c -> c >= 'a' && c <= 'z')

upper :: Parser Char
upper = satC (\c -> c >= 'A' && c <= 'Z')

letter :: Parser Char
letter = lower <|> upper

alphanum :: Parser Char
alphanum = digit <|> letter

first_third :: Parser (Char, Char)
first_third = do { x <- item
                 ; _ <- item
                 ; y <- item
                 ; return (x, y) }