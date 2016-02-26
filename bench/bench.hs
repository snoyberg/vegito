import Criterion.Main

import qualified Conduit as C
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VB
import Vegito
import Gotenks
import Data.Functor.Identity

main :: IO ()
main = defaultMain
    [ bgroup "sum $ map (+ 1) $ map (* 2) $ enumFromTo 1 9001"
        [ bench' "vegito" $ \x ->
                  runIdentity
                $ sumV
                $ mapV (+ 1)
                $ mapV (* 2)
                $ enumFromToV 1 x
        , bench' "gotenks" $ \x ->
                  runIdentity
                $ toSink sumG
                $ toTransform (mapG (+ 1))
                $ toTransform (mapG (* 2))
                $ toSource (enumFromToG 1 x)
        , bench' "conduit-combinators" $ \x ->
                  runIdentity
                $ C.enumFromToC 1 x
             C.$= C.mapC (* 2)
             C.$= C.mapC (+ 1)
             C.$$ C.sumC
        , bench' "vector boxed" $ \x ->
                  VB.sum
                $ VB.map (+ 1)
                $ VB.map (* 2)
                $ VB.enumFromTo 1 x
        , bench' "vector unboxed" $ \x ->
                  V.sum
                $ V.map (+ 1)
                $ V.map (* 2)
                $ V.enumFromTo 1 x
        , bench' "vector unboxed foldM" $ \x ->
                  runIdentity
                $ V.foldM (\total i -> return $! total + i) 0
                $ V.map (+ 1)
                $ V.map (* 2)
                $ V.enumFromTo 1 x
        ]
    ]
  where
    bench' :: String -> (Int -> Int) -> Benchmark
    bench' name f = bench name (whnf f (9001 :: Int))
