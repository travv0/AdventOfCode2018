module Main where
import           Data.List

main :: IO ()
main = interact (show . findMatch . lines)

findMatch :: [String] -> String
findMatch ids =
  let idLen = length (head ids)
  in  head $ filter (\s -> length s == idLen - 1)
                    (concat $ map (getMatchingChars ids) ids)

getMatchingChars :: [String] -> String -> [String]
getMatchingChars ids id = map
  (\compId ->
    foldr (\(c1, c2) s -> if c1 == c2 then c1 : s else s) [] $ zip id compId
  )
  ids
