module Main where
import           Data.List

main :: IO ()
main = interact (show . generateChecksum . lines)

generateChecksum :: [String] -> Int
generateChecksum ids =
  let tats           = map hasTwosAndThrees ids
      (twos, threes) = foldr
        (\(b2, b3) (tws, ths) ->
          (if b2 then tws + 1 else tws, if b3 then ths + 1 else ths)
        )
        (0, 0)
        tats
  in  twos * threes

hasTwosAndThrees :: String -> (Bool, Bool)
hasTwosAndThrees s =
  let counts = map (\c -> length $ filter (== c) s) $ nub s
  in  (2 `notElem` counts, 3 `notElem` counts)
