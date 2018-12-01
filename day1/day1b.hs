module Main where

getFirstRepeatingFrequency :: [String] -> Integer
getFirstRepeatingFrequency freqs =
  getFirstRepeatingFrequency' [0] 0 freqs freqs

getFirstRepeatingFrequency'
  :: [Integer] -> Integer -> [String] -> [String] -> Integer
getFirstRepeatingFrequency' seenFreqs total freqs allFreqs =
  let freq         = head freqs
      freqSign     = head freq
      unsignedFreq = read (tail freq)
      newFreq =
        if freqSign == '-' then total - unsignedFreq else total + unsignedFreq
  in  case newFreq `elem` seenFreqs of
        True  -> newFreq
        False -> getFirstRepeatingFrequency'
          (newFreq : seenFreqs)
          newFreq
          (if null (tail freqs) then allFreqs else tail freqs)
          allFreqs

main :: IO ()
main = interact (show . getFirstRepeatingFrequency . lines)
