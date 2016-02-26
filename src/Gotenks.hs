{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module Gotenks where

import Control.Monad (ap)
import Control.Applicative (liftA2)
import Data.Void (Void, absurd)
import Vegito

data GotenksF i o m r
    = YieldF o (GotenksF i o m r)
    | AwaitF (i -> GotenksF i o m r) (GotenksF i o m r)
    | DoF (m (GotenksF i o m r))
    | DoneF r
    | LeftoverF i (GotenksF i o m r)
  deriving Functor

newtype Gotenks i o m r = Gotenks
    { unGotenks :: forall b.
                   (r -> GotenksF i o m b) -> GotenksF i o m b
    }
  deriving Functor
instance Applicative (Gotenks i o m) where
    pure r = Gotenks (\f -> f r)
    (<*>) = ap
instance Monad (Gotenks i o m) where
    return = pure
    (>>) = (*>)

    Gotenks f >>= g = Gotenks (\h -> f $ \a -> unGotenks (g a) h)
instance Monoid r => Monoid (Gotenks i o m r) where
    mempty = pure mempty
    mappend = liftA2 mappend

await :: Gotenks i o m (Maybe i)
await = Gotenks (\f -> AwaitF (f . Just) (f Nothing))
{-# INLINE await #-}

yield :: o -> Gotenks i o m ()
yield o = Gotenks (\f -> YieldF o (f ()))
{-# INLINE yield #-}

leftover :: i -> Gotenks i o m ()
leftover i = Gotenks (\f -> LeftoverF i (f ()))
{-# INLINE leftover #-}

runGotenks :: Monad m => Gotenks () Void m r -> m r
runGotenks (Gotenks orig) =
    loop (orig DoneF)
  where
    loop (YieldF o _) = absurd o
    loop (AwaitF _ f) = loop f
    loop (DoF m) = m >>= loop
    loop (DoneF r) = pure r
    loop (LeftoverF () f) = loop f

toSource :: Applicative m => Gotenks () o m r -> Source o m r
toSource (Gotenks orig) =
    Source go (orig DoneF)
  where
    go (YieldF o f) = pure (Yield f o)
    go (AwaitF _ f) = pure (Skip f)
    go (DoF f) = fmap Skip f
    go (DoneF r) = pure (Done r)
    go (LeftoverF () r) = pure (Skip r)
{-# INLINE [0] toSource #-}

toSink :: Monad m => Gotenks i Void m r -> Source i m () -> m r
toSink (Gotenks forig) (Source step sorig) =
    let loop _ (DoneF r) _ = pure r
        loop _ (YieldF o _) _ = absurd o
        loop is (LeftoverF i f) s = loop (i:is) f s
        loop is (DoF f) s = f >>= \f' -> loop is f' s
        loop (i:is) (AwaitF f _) s = loop is (f i) s
        loop [] (AwaitF f g) s = do
            x <- step s
            case x of
                Yield s' i -> loop [] (f i) s'
                Skip s' -> loop [] (AwaitF f g) s'
                Done () -> finish [] g

        finish _ (DoneF r) = pure r
        finish _ (YieldF o _) = absurd o
        finish is (LeftoverF i f) = finish (i:is) f
        finish is (DoF f) = f >>= \f' -> finish is f'
        finish [] (AwaitF _ f) = finish [] f
        finish (i:is) (AwaitF f _) = finish is (f i)
     in loop [] (forig DoneF) sorig
{-# INLINE [0] toSink #-}

toTransform :: Applicative m => Gotenks i o m r -> Source i m () -> Source o m r
toTransform (Gotenks forig) (Source step sorig) =
    Source go ([], Just sorig, forig DoneF)
  where
    go (_, _, DoneF r) = pure (Done r)
    go (is, s, YieldF o f) = pure (Yield (is, s, f) o)
    go (is, s, LeftoverF i f) = pure (Skip (i:is, s, f))
    go (is, s, DoF f) = fmap (\f' -> Skip (is, s, f')) f
    go (i:is, s, AwaitF f _) = pure (Skip (is, s, f i))
    go ([], Just s, AwaitF f g) = do
        fmap go' (step s)
      where
        go' (Yield s' i) = Skip ([], Just s', f i)
        go' (Skip s') = Skip ([], Just s', AwaitF f g)
        go' (Done ()) = Skip ([], Nothing, g)
    go ([], Nothing, AwaitF _ f) = pure (Skip ([], Nothing, f))
{-# INLINE [0] toTransform #-}

enumFromToG :: (Ord o, Num o) => o -> o -> Gotenks i o m ()
enumFromToG low high =
    loop low
  where
    loop x
        | x <= high = yield x >> (loop $! x + 1)
        | otherwise = pure ()
{-# INLINE [0] enumFromToG #-}
{-# RULES "toSource enumFromToG"
    forall x y. toSource (enumFromToG x y) = enumFromToV x y
  #-}

mapG :: (i -> o) -> Gotenks i o m ()
mapG f =
    loop
  where
    loop = do
        mi <- await
        case mi of
            Nothing -> pure ()
            Just i -> yield (f i) *> loop
{-# INLINE [0] mapG #-}
{-# RULES "toTransform mapG"
    forall f. toTransform (mapG f) = mapV f
  #-}

foldlG :: (r -> i -> r) -> r -> Gotenks i o m r
foldlG f accum0 =
    loop accum0
  where
    loop accum = do
        mi <- await
        case mi of
            Nothing -> pure accum
            Just i ->
                let accum' = f accum i
                 in accum' `seq` loop accum'
{-# INLINE [0] foldlG #-}
{-# RULES "toSink foldlG"
    forall f accum. toSink (foldlG f accum) = foldlV f accum
  #-}

sumG :: Num i => Gotenks i o m i
sumG = foldlG (+) 0
{-# INLINE sumG #-}
