module VegitoSpec (main, spec) where

import Test.Hspec

import Vegito
import Data.Functor.Identity

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sanity" $ do
    it "map and sum" $
      runIdentity (sumS $ mapS (+ 1) $ mapS (* 2) $ enumFromToS 1 9001)
        `shouldBe` sum (map (+ 1) $ map (* 2) [1..9001 :: Int])
