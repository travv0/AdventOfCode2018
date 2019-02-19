import           Test.Hspec
import           Test.Hspec.Expectations

import           Lib

main :: IO ()
main = hspec $ describe "runGame" $ do
  it "works with given test cases" $ do
    runGame "#######\n#G..#E#\n#E#E.E#\n#G.##.#\n#...#E#\n#...E.#\n#######"
      `shouldBe` 36334

    runGame "#######\n#E..EG#\n#.#G.E#\n#E.##E#\nEG..#.#\n#..E#.#\n#######"
      `shouldBe` 39514
