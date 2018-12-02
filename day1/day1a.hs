module Main where

sumFrequencies :: [String] -> Integer
sumFrequencies = foldr
  (\freq total ->
    let freqSign     = head freq
        unsignedFreq = read (tail freq)
    in  if freqSign == '-' then total - unsignedFreq else total + unsignedFreq
  )
  0

main :: IO ()
main = interact (show . sumFrequencies . lines)
