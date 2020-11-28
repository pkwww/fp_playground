import Data.List
import Control.Monad
import Control.Applicative


newtype Fix f = Fix { unFix :: f (Fix f) }
cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . unFix

newtype LM a = LM{ getLM :: [Maybe a ]}

instance Functor LM where
  fmap f lma = LM $ (fmap . fmap) f (getLM lma)

instance Applicative LM where
  pure a = LM [Just a]
  (LM fs) <*> (LM as) = LM $ pure (<*>) <*> fs <*> as

(+++) :: LM a -> LM a -> LM a
a +++ b = LM (getLM a ++ getLM b)

newf :: (a -> LM b) -> Maybe a -> LM b
newf f Nothing = LM [Nothing]
newf f (Just a) = f a

instance Monad LM where
  return a = LM [Just a]
  -- >>= :: LM a -> (a -> LM b) -> LM b
  (LM []) >>= _ = LM []
  (LM (a : as)) >>= f = newf f a +++ ((LM as) >>= f)
