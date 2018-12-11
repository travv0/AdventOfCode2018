module Main where

getFirstRepeatingFrequency :: [String] -> Integer
getFirstRepeatingFrequency freqs =
  getFirstRepeatingFrequency' [0] 0 freqs freqs

getFirstRepeatingFrequency'
  :: [Integer] -> Integer -> [String] -> [String] -> Integer
getFirstRepeatingFrequency' seenFreqs total freqs allFreqs =
  let freq           = head freqs
      remainingFreqs = tail freqs
      freqSign       = head freq
      unsignedFreq   = read (tail freq)
      newFreq =
        if freqSign == '-' then total - unsignedFreq else total + unsignedFreq
  in  if newFreq `elem` seenFreqs
        then newFreq
        else getFirstRepeatingFrequency'
          (newFreq : seenFreqs)
          newFreq
          (if null remainingFreqs then allFreqs else remainingFreqs)
          allFreqs

main :: IO ()
main = interact (show . getFirstRepeatingFrequency . lines)
