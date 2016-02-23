{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Vegito where

data Source o m r where
    Source :: (s -> m (Step s o r)) -> s -> Source o m r
instance (Applicative m, Monoid r) => Monoid (Source o m r) where
    mempty =
        Source f ()
      where
        f () = pure (Done mempty)

    mappend (Source f1 s1orig) (Source f2 s2orig) =
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
            goStep (Done r2) = Done (mappend r1 r2)

data MappendHelper x y r = First x y | Second r y

data Step s o r where
    Yield :: s -> o -> Step s o r
    Skip :: s -> Step s o r
    Done :: r -> Step s o r

enumFromToV :: (Ord o, Applicative m, Num o) => o -> o -> Source o m ()
enumFromToV low high =
    Source go low
  where
    go x
        | x <= high = pure (Yield (x + 1) x)
        | otherwise = pure (Done ())
{-# INLINE enumFromToV #-}

sumV :: (Num i, Monad m) => Source i m () -> m i
sumV (Source f sorig) =
    let loop !total s = do
            step <- f s
            case step of
                Done () -> pure total
                Skip s' -> loop total s'
                Yield s' i ->
                    let total' = total + i
                     in total' `seq` loop total' s'
     in loop 0 sorig
{-# INLINE sumV #-}

mapV :: Functor m => (i -> o) -> Source i m r -> Source o m r
mapV f (Source src sorig) =
    let go s = fmap goStep (src s)

        goStep (Yield s i) = Yield s (f i)
        goStep (Skip s) = Skip s
        goStep (Done r) = Done r

     in Source go sorig
{-# INLINE mapV #-}
