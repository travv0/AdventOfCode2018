module Main where
import           Data.List

main :: IO ()
main = interact (show . generateChecksum . lines)

generateChecksum :: [String] -> Int
generateChecksum ids =
  let tats           = map hasTwosAndThrees ids
      (twos, threes) = foldr
        (\(b2, b3) (twos, threes) ->
          (if b2 then twos + 1 else twos, if b3 then threes + 1 else threes)
        )
        (0, 0)
        tats
  in  twos * threes

hasTwosAndThrees :: String -> (Bool, Bool)
hasTwosAndThrees id =
  let counts = map (\c -> length $ filter (\x -> x == c) id) $ nub id
  in  ( length (filter (\x -> x == 2) counts) > 0
      , length (filter (\x -> x == 3) counts) > 0
      )
