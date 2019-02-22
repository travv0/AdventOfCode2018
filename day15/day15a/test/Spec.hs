import           Control.Monad.State
import           Test.Hspec
import qualified Data.HashSet                  as H
import qualified Data.Vector                   as V

import           Lib

main :: IO ()
main = hspec $ do
  describe "parseInput"
    $          it "correctly parses input into cells"
    $          parseInput "#.GE#\n##.G#"
    `shouldBe` V.fromList
                 [ Cell Wall                               (0, 0)
                 , Cell Cavern                             (1, 0)
                 , Cell (Unit (UnitAttr Goblin 200 False)) (2, 0)
                 , Cell (Unit (UnitAttr Elf 200 False))    (3, 0)
                 , Cell Wall                               (4, 0)
                 , Cell Wall                               (0, 1)
                 , Cell Wall                               (1, 1)
                 , Cell Cavern                             (2, 1)
                 , Cell (Unit (UnitAttr Goblin 200 False)) (3, 1)
                 , Cell Wall                               (4, 1)
                 ]

  describe "neighbors"
    $          it "returns correct neighbors in correct order"
    $          evalState
                 (neighbors (Cell Cavern (1, 0)))
                 (BattleState
                   { _mapWidth    = 5
                   , _battleMap   = parseInput "#..E#\n#..G#"
                   , _battleRound = 0
                   }
                 )
    `shouldBe` H.fromList
                 [Cell Wall (0, 0), Cell Cavern (2, 0), Cell Cavern (1, 1)]

  describe "distance"
    $ it "correctly calculates the manhattan distance between two points"
    $ distance (Cell Wall (1, 8)) (Cell Wall (5, 1))
    `shouldBe` 11

  describe "moveUnit"
    $ it "correctly moves units per advent of code examples"
    $ do
        _battleMap
            (execState
              (do
                targets <- getSortedTargets
                  (Cell (Unit (UnitAttr Elf 200 False)) (2, 1))
                moveUnit (Cell (Unit (UnitAttr Elf 200 False)) (2, 1)) targets
              )
              BattleState
                { _battleRound = 0
                , _mapWidth    = 7
                , _battleMap   = parseInput
                  $  "#######\n"
                  ++ "#.E...#\n"
                  ++ "#.....#\n"
                  ++ "#...G.#\n"
                  ++ "#######"
                }
            )
          `shouldBe` parseInput
                       (  "#######\n"
                       ++ "#..E..#\n"
                       ++ "#.....#\n"
                       ++ "#...G.#\n"
                       ++ "#######"
                       )

        _battleMap
            (execState
              (do
                targets <- getSortedTargets
                  (Cell (Unit (UnitAttr Elf 200 False)) (1, 1))
                moveUnit (Cell (Unit (UnitAttr Elf 200 False)) (1, 1)) targets
              )
              BattleState
                { _battleRound = 0
                , _mapWidth    = 7
                , _battleMap   = parseInput
                  $  "#######\n"
                  ++ "#E..G.#\n"
                  ++ "#...#.#\n"
                  ++ "#.G.#G#\n"
                  ++ "#######"
                }
            )
          `shouldBe` parseInput
                       (  "#######\n"
                       ++ "#.E.G.#\n"
                       ++ "#...#.#\n"
                       ++ "#.G.#G#\n"
                       ++ "#######"
                       )

        _battleMap
            (execState
              (do
                targets <- getSortedTargets
                  (Cell (Unit (UnitAttr Elf 200 False)) (1, 1))
                moveUnit (Cell (Unit (UnitAttr Elf 200 False)) (1, 1)) targets
              )
              BattleState
                { _battleRound = 0
                , _mapWidth    = 7
                , _battleMap   = parseInput
                  $  "#######\n"
                  ++ "#E..#G#\n"
                  ++ "#...#.#\n"
                  ++ "#...#G#\n"
                  ++ "#######"
                }
            )
          `shouldBe` parseInput
                       (  "#######\n"
                       ++ "#E..#G#\n"
                       ++ "#...#.#\n"
                       ++ "#...#G#\n"
                       ++ "#######"
                       )

        _battleMap
            (execState
              (do
                targets <- getSortedTargets
                  (Cell (Unit (UnitAttr Elf 200 False)) (1, 1))
                moveUnit (Cell (Unit (UnitAttr Elf 200 False)) (1, 1)) targets
              )
              BattleState
                { _battleRound = 0
                , _mapWidth    = 7
                , _battleMap   = parseInput
                  $  "#######\n"
                  ++ "#EG.#G#\n"
                  ++ "#...#.#\n"
                  ++ "#...#G#\n"
                  ++ "#######"
                }
            )
          `shouldBe` parseInput
                       (  "#######\n"
                       ++ "#EG.#G#\n"
                       ++ "#...#.#\n"
                       ++ "#...#G#\n"
                       ++ "#######"
                       )

  describe "runTurn"
    $ it "correctly updates state in one turn based on examples"
    $ let
        initialMap =
          parseInput
            $  "#########\n"
            ++ "#G..G..G#\n"
            ++ "#.......#\n"
            ++ "#.......#\n"
            ++ "#G..E..G#\n"
            ++ "#.......#\n"
            ++ "#.......#\n"
            ++ "#G..G..G#\n"
            ++ "#########"
        round1Map =
          parseInput
            $  "#########\n"
            ++ "#.G...G.#\n"
            ++ "#...G...#\n"
            ++ "#...E..G#\n"
            ++ "#.G.....#\n"
            ++ "#.......#\n"
            ++ "#G..G..G#\n"
            ++ "#.......#\n"
            ++ "#########"
        round2Map =
          parseInput
            $  "#########\n"
            ++ "#..G.G..#\n"
            ++ "#...G...#\n"
            ++ "#.G.E.G.#\n"
            ++ "#.......#\n"
            ++ "#G..G..G#\n"
            ++ "#.......#\n"
            ++ "#.......#\n"
            ++ "#########"
        round3Map =
          parseInput
            $  "#########\n"
            ++ "#.......#\n"
            ++ "#..GGG..#\n"
            ++ "#..GEG..#\n"
            ++ "#G..G...#\n"
            ++ "#......G#\n"
            ++ "#.......#\n"
            ++ "#.......#\n"
            ++ "#########"
      in
        do
          showMap
              9
              (_battleMap
                (execState
                  runTurn
                  (BattleState
                    { _battleRound = 0
                    , _mapWidth    = 9
                    , _battleMap   = initialMap
                    }
                  )
                )
              )
            `shouldBe` showMap
                         9
                         (_battleMap
                           (BattleState
                             { _battleRound = 1
                             , _mapWidth    = 9
                             , _battleMap   = round1Map
                             }
                           )
                         )

          showMap
              9
              (_battleMap
                (execState
                  runTurn
                  (BattleState
                    { _battleRound = 1
                    , _mapWidth    = 9
                    , _battleMap   = round1Map
                    }
                  )
                )
              )
            `shouldBe` showMap
                         9
                         (_battleMap
                           (BattleState
                             { _battleRound = 2
                             , _mapWidth    = 9
                             , _battleMap   = round2Map
                             }
                           )
                         )

          showMap
              9
              (_battleMap
                (execState
                  runTurn
                  (BattleState
                    { _battleRound = 2
                    , _mapWidth    = 9
                    , _battleMap   = round2Map
                    }
                  )
                )
              )
            `shouldBe` showMap
                         9
                         (_battleMap
                           (BattleState
                             { _battleRound = 3
                             , _mapWidth    = 9
                             , _battleMap   = round3Map
                             }
                           )
                         )

  describe "attack" $ do
    it "attacks weakest neighbor enemy unit" $ do
      let initialState = BattleState
            { _battleRound = 0
            , _mapWidth    = 2
            , _battleMap   = V.fromList
              [ Cell Cavern                            (0, 0)
              , Cell (Unit (UnitAttr Goblin 200 True)) (1, 0)
              , Cell (Unit (UnitAttr Goblin 180 True)) (0, 1)
              , Cell (Unit (UnitAttr Elf 200 True))    (1, 1)
              ]
            }
      execState (attack (Cell (Unit (UnitAttr Elf 200 True)) (1, 1)))
                initialState
        `shouldBe` (BattleState
                     { _battleRound = 0
                     , _mapWidth    = 2
                     , _battleMap   = V.fromList
                       [ Cell Cavern                            (0, 0)
                       , Cell (Unit (UnitAttr Goblin 200 True)) (1, 0)
                       , Cell (Unit (UnitAttr Goblin 177 True)) (0, 1)
                       , Cell (Unit (UnitAttr Elf 200 True))    (1, 1)
                       ]
                     }
                   )

    it "kills off (removes) unit with 0 or less health" $ do
      let initialState = BattleState
            { _battleRound = 0
            , _mapWidth    = 2
            , _battleMap   = V.fromList
              [ Cell Cavern                            (0, 0)
              , Cell (Unit (UnitAttr Goblin 3 True))   (1, 0)
              , Cell (Unit (UnitAttr Goblin 180 True)) (0, 1)
              , Cell (Unit (UnitAttr Elf 200 True))    (1, 1)
              ]
            }
      execState (attack (Cell (Unit (UnitAttr Elf 200 True)) (1, 1)))
                initialState
        `shouldBe` (BattleState
                     { _battleRound = 0
                     , _mapWidth    = 2
                     , _battleMap   = V.fromList
                       [ Cell Cavern                            (0, 0)
                       , Cell Cavern                            (1, 0)
                       , Cell (Unit (UnitAttr Goblin 180 True)) (0, 1)
                       , Cell (Unit (UnitAttr Elf 200 True))    (1, 1)
                       ]
                     }
                   )

  describe "calculateResult"
    $          it "calculates correctly"
    $          calculateResult BattleState
                 { _battleRound = 12
                 , _mapWidth    = 3
                 , _battleMap   = V.fromList
                   [ Cell Cavern                            (0, 0)
                   , Cell Cavern                            (1, 0)
                   , Cell (Unit (UnitAttr Goblin 180 True)) (0, 1)
                   , Cell (Unit (UnitAttr Elf 200 True))    (1, 1)
                   ]
                 }
    `shouldBe` 4560

  describe "startGame" $ it "works with given test cases" $ do
    startGame "#######\n#G..#E#\n#E#E.E#\n#G.##.#\n#...#E#\n#...E.#\n#######"
      `shouldBe` 36334

    startGame "#######\n#E..EG#\n#.#G.E#\n#E.##E#\nEG..#.#\n#..E#.#\n#######"
      `shouldBe` 39514

    startGame "#######\n#E.G#.#\n#.#G..#\n#G.#.G#\n#G..#.#\n#...E.#\n#######"
      `shouldBe` 27755

    startGame "#######\n#.E...#\n#.#..G#\n#.###.#\n#E#G#G#\n#...#G#\n#######"
      `shouldBe` 28944

    startGame
        "#########\n#G......#\n#.E.#...#\n#..##..G#\n#...##..#\n#...#...#\n#.G...G.#\n#.....G.#\n#########"
      `shouldBe` 18740
