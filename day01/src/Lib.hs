module Lib where

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

sumFrequencies :: [String] -> Integer
sumFrequencies =
    foldr
        ( \freq total ->
            let freqSign = head freq
                unsignedFreq = read (tail freq)
             in if freqSign == '-' then total - unsignedFreq else total + unsignedFreq
        )
        0

getFirstRepeatingFrequency :: [String] -> Integer
getFirstRepeatingFrequency freqs =
    getFirstRepeatingFrequency' (HashSet.fromList [0]) 0 freqs freqs

getFirstRepeatingFrequency' :: HashSet Integer -> Integer -> [String] -> [String] -> Integer
getFirstRepeatingFrequency' seenFreqs total freqs allFreqs =
    let freq = head freqs
        remainingFreqs = tail freqs
        freqSign = head freq
        unsignedFreq = read (tail freq)
        newFreq =
            if freqSign == '-' then total - unsignedFreq else total + unsignedFreq
     in if newFreq `HashSet.member` seenFreqs
            then newFreq
            else
                getFirstRepeatingFrequency'
                    (HashSet.insert newFreq seenFreqs)
                    newFreq
                    (if null remainingFreqs then allFreqs else remainingFreqs)
                    allFreqs