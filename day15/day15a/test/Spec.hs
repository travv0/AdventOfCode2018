import           Control.Monad.State
import           Test.Hspec
import qualified Data.HashSet                  as H
import qualified Data.Vector                   as V

import           Lib

main :: IO ()
main = hspec $ do
  describe "runGame" $ it "works with given test cases" $ do
    runGame "#######\n#G..#E#\n#E#E.E#\n#G.##.#\n#...#E#\n#...E.#\n#######"
      `shouldBe` 36334

    runGame "#######\n#E..EG#\n#.#G.E#\n#E.##E#\nEG..#.#\n#..E#.#\n#######"
      `shouldBe` 39514

    runGame "#######\n#E.G#.#\n#.#G..#\n#G.#.G#\n#G..#.#\n#...E.#\n#######"
      `shouldBe` 27755

    runGame "#######\n#.E...#\n#.#..G#\n#.###.#\n#E#G#G#\n#...#G#\n#######"
      `shouldBe` 28944

    runGame
        "#########\n#G......#\n#.E.#...#\n#..##..G#\n#...##..#\n#...#...#\n#.G...G.#\n#.....G.#\n#########"
      `shouldBe` 18740

  describe "parseInput"
    $          it "correctly parses input into cells"
    $          parseInput "#.GE#\n##.G#"
    `shouldBe` V.fromList
                 [ V.fromList
                   [ Cell Wall   (0, 0)
                   , Cell Cavern (1, 0)
                   , Cell Goblin (2, 0)
                   , Cell Elf    (3, 0)
                   , Cell Wall   (4, 0)
                   ]
                 , V.fromList
                   [ Cell Wall   (0, 1)
                   , Cell Wall   (1, 1)
                   , Cell Cavern (2, 1)
                   , Cell Goblin (3, 1)
                   , Cell Wall   (4, 1)
                   ]
                 ]

  describe "neighbors"
    $ it "returns correct neighbors in correct order"
    $ evalState (neighbors (Cell Cavern (1, 0))) (parseInput "#.GE#\n##.G#")
    `shouldBe` H.fromList
                 [Cell Wall (0, 0), Cell Goblin (2, 0), Cell Wall (1, 1)]

  describe "distance"
    $ it "correctly calculates the manhattan distance between two points"
    $ distance (Cell Wall (1, 8)) (Cell Wall (5, 1))
    `shouldBe` 11
