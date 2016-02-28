{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
module Vegito where

import Control.Applicative (liftA2)

data Step s o r where
    Yield :: s -> o -> Step s o r
    Skip :: s -> Step s o r
    Done :: r -> Step s o r
  deriving Functor

data Stream o m r where
    Stream :: (s -> m (Step s o r)) -> s -> Stream o m r
instance Functor m => Functor (Stream o m) where
    fmap f (Stream step s) = Stream (fmap (fmap f) . step) s
instance Applicative m => Applicative (Stream o m) where
    pure r = Stream (\() -> pure (Done r)) ()
    Stream f1 s1orig <*> Stream f2 s2orig =
        Stream go (First s1orig s2orig)
      where
        go (First s1 s2) =
            fmap goStep (f1 s1)
          where
            goStep (Yield s1' o) = Yield (First s1' s2) o
            goStep (Skip s1') = Skip (First s1' s2)
            goStep (Done r) = Skip (Second r s2)

        go (Second r1 s2) =
            fmap goStep (f2 s2)
          where
            goStep (Yield s2' o) = Yield (Second r1 s2') o
            goStep (Skip s2') = Skip (Second r1 s2')
            goStep (Done r2) = Done (r1 r2)

data ApplicativeHelper x y r = First x y | Second r y

instance Applicative m => Monad (Stream o m) where
    return = pure
    (>>) = (*>)
    Stream f1 s1orig >>= right =
        Stream go (Left s1orig)
      where
        go (Left s1) =
            fmap goStep (f1 s1)
          where
            goStep (Yield s1' o) = Yield (Left s1') o
            goStep (Skip s1') = Skip (Left s1')
            goStep (Done r) = Skip (Right (right r))

        go (Right (Stream f2 s2)) =
            fmap goStep (f2 s2)
          where
            goStep (Yield s2' o) = Yield (Right (Stream f2 s2')) o
            goStep (Skip s2') = Skip (Right (Stream f2 s2'))
            goStep (Done r) = Done r

instance (Applicative m, Monoid r) => Monoid (Stream o m r) where
    mempty = pure mempty
    mappend = liftA2 mappend

enumFromToS :: (Ord o, Applicative m, Num o) => o -> o -> Stream o m ()
enumFromToS low high =
    Stream go low
  where
    go x
        | x <= high = pure (Yield (x + 1) x)
        | otherwise = pure (Done ())
{-# INLINE enumFromToS #-}

foldlS :: (Monad m) => (r -> i -> r) -> r -> Stream i m () -> m r
foldlS g accum0 (Stream f sorig) =
    let loop accum s = do
            step <- f s
            case step of
                Done () -> pure accum
                Skip s' -> loop accum s'
                Yield s' i ->
                    let accum' = g accum i
                     in accum' `seq` loop accum' s'
     in loop accum0 sorig
{-# INLINE foldlS #-}

sumS :: (Num i, Monad m) => Stream i m () -> m i
sumS = foldlS (+) 0
{-# INLINE sumS #-}

mapS :: Functor m => (i -> o) -> Stream i m r -> Stream o m r
mapS f (Stream src sorig) =
    let go s = fmap goStep (src s)

        goStep (Yield s i) = Yield s (f i)
        goStep (Skip s) = Skip s
        goStep (Done r) = Done r

     in Stream go sorig
{-# INLINE mapS #-}
