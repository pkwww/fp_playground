module PkwParser where

import Control.Monad
import Control.Applicative hiding (many)

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
p +++ q = P $ \input -> case parse (p <|> q) input of
                        [] -> []
                        (x:_) -> [x]

seq :: Parser a -> Parser b -> Parser (a, b)
p `seq` q = do { x <- p
               ; y <- q
               ; return (x, y) }

item :: Parser Char
item = P $ \input -> case input of
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

word :: Parser String
word = neword <|> return ""
        where neword = do { x <- letter
                          ; xs <- word
                          ; return (x:xs)}

word' :: Parser String
word' = many letter

many :: Parser a -> Parser [a]
many p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = do { x <- p
             ; xs <- many p
             ; return (x:xs)}

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = p `chainl1` op <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do { a <- p; rest a }
                 where
                    rest a = do { f <- op; b <- p; rest (f a b) } <|> return a

first_third :: Parser (Char, Char)
first_third = do { x <- item
                 ; _ <- item
                 ; y <- item
                 ; return (x, y) }