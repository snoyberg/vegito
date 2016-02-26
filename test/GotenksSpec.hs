module GotenksSpec (main, spec) where

import Test.Hspec

import Gotenks
import Data.Functor.Identity

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "sanity check" $
        let res = runIdentity
                $ toSink sumG
                $ toTransform (mapG (+ 1))
                $ toTransform (mapG (* 2))
                $ toSource (enumFromToG 1 9001)
         in res `shouldBe` sum (map (+ 1) $ map (* 2) [1..9001 :: Int])
