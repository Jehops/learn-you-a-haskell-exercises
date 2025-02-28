-- This function should print a single digit number as English text, or "unknown" if it's out of the range 0-9
englishDigit :: Int -> String
englishDigit x = case x of 1 -> "one"
                           2 -> "two"
                           3 -> "three"
                           4 -> "four"
                           5 -> "five"
                           6 -> "six"
                           7 -> "seven"
                           8 -> "eight"
                           9 -> "nine"

-- given a tuple, divide fst by snd, using pattern matching.
-- it should return undefined for division by zero
divTuple :: (Eq a, Fractional a) => (a, a) -> a
divTuple (x, 0) = undefined
divTuple (x, y) = x/y

-- if the first three numbers in a list are all zero, return True
threeZeroList :: [Int] -> Bool
threeZeroList (0:0:0:xs) = True
threeZeroList _ = False
