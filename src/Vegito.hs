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

data Source o m r where
    Source :: (s -> m (Step s o r)) -> s -> Source o m r
instance Functor m => Functor (Source o m) where
    fmap f (Source step s) = Source (fmap (fmap f) . step) s
instance Applicative m => Applicative (Source o m) where
    pure r = Source (\() -> pure (Done r)) ()
    Source f1 s1orig <*> Source f2 s2orig =
        Source go (First s1orig s2orig)
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

instance Applicative m => Monad (Source o m) where
    return = pure
    (>>) = (*>)
    Source f1 s1orig >>= right =
        Source go (Left s1orig)
      where
        go (Left s1) =
            fmap goStep (f1 s1)
          where
            goStep (Yield s1' o) = Yield (Left s1') o
            goStep (Skip s1') = Skip (Left s1')
            goStep (Done r) = Skip (Right (right r))

        go (Right (Source f2 s2)) =
            fmap goStep (f2 s2)
          where
            goStep (Yield s2' o) = Yield (Right (Source f2 s2')) o
            goStep (Skip s2') = Skip (Right (Source f2 s2'))
            goStep (Done r) = Done r

instance (Applicative m, Monoid r) => Monoid (Source o m r) where
    mempty = pure mempty
    mappend = liftA2 mappend

enumFromToV :: (Ord o, Applicative m, Num o) => o -> o -> Source o m ()
enumFromToV low high =
    Source go low
  where
    go x
        | x <= high = pure (Yield (x + 1) x)
        | otherwise = pure (Done ())
{-# INLINE enumFromToV #-}

foldlV :: (Num i, Monad m) => (r -> i -> r) -> r -> Source i m () -> m r
foldlV g accum0 (Source f sorig) =
    let loop accum s = do
            step <- f s
            case step of
                Done () -> pure accum
                Skip s' -> loop accum s'
                Yield s' i ->
                    let accum' = g accum i
                     in accum' `seq` loop accum' s'
     in loop accum0 sorig
{-# INLINE foldlV #-}

sumV :: (Num i, Monad m) => Source i m () -> m i
sumV = foldlV (+) 0
{-# INLINE sumV #-}

mapV :: Functor m => (i -> o) -> Source i m r -> Source o m r
mapV f (Source src sorig) =
    let go s = fmap goStep (src s)

        goStep (Yield s i) = Yield s (f i)
        goStep (Skip s) = Skip s
        goStep (Done r) = Done r

     in Source go sorig
{-# INLINE mapV #-}
