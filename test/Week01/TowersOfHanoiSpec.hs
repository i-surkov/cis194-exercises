module Week01.TowersOfHanoiSpec
  ( spec,
    hspec,
  )
where

import Test.Hspec
  ( Spec,
    describe,
    hspec,
    it,
    shouldBe,
  )
import Week01.TowersOfHanoi (hanoi, hanoi4)

spec :: Spec
spec = describe "TowersOfHanoi" $ do
  hanoiSpec
  hanoi4Spec

hanoiSpec :: Spec
hanoiSpec =
  describe "hanoi" $ do
    it "lists the moves needed to move _n_ disks from peg a to peg b" $
      do
        hanoi 2 "a" "b" "c" `shouldBe` [("a", "c"), ("a", "b"), ("c", "b")]
        hanoi 3 "a" "b" "c"
          `shouldBe` [ ("a", "b"),
                       ("a", "c"),
                       ("b", "c"),
                       ("a", "b"),
                       ("c", "a"),
                       ("c", "b"),
                       ("a", "b")
                     ]
    it "produces 32767 moves for 15-element tower" $
      length (hanoi 15 "a" "b" "c") `shouldBe` 32767

hanoi4Spec :: Spec
hanoi4Spec =
  describe "hanoi4" $
    it "produces 129 moves for 15-element tower" $
      length (hanoi4 15 "a" "b" "c" "d") `shouldBe` 129