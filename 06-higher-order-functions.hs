-- Chapter Examples

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myFlip :: (a -> b -> c) -> (b -> a -> c)
myFlip f x y = f y x

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smaller = quicksort (filter (<x) xs)
      larger = quicksort (filter (>=x) xs)
  in smaller ++ [x] ++ larger

-- Find the largest number under 100,000 that is divisible by 3892.

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [99999,99998..1])
  where p x = x `mod` 3892 == 0

-- Find the sum of all odd squares that are smaller than 10,000

-- This doesn't work because list comprehensions will generate generate values one by one but
-- do not stop automatically when the condition is no longer satisfied, leading
-- to an infinite loop.

sumOddSquares :: (Integral a) => a
sumOddSquares = sum([x^2 | x <- [1,3..], x^2 < 10000])

-- For a solution with a list comprehension, do this instead
sumOddSquares' :: (Integral a) => a
sumOddSquares' = sum(takeWhile (<10000) [x^2 | x <- [1,3..]])

sumOddSquares2 :: (Integral a) => a
sumOddSquares2 = sum (takeWhile (<10000) (map (^2) [1,3..]))

-- To generate a Collatz sequences, we start with a natural number. If that
-- number is even, we divide it by two.  If it's odd, we multiply it by 3 and
-- then add 1.  We take the resulting number and apply the same thing to it,
-- which produces a new number and so on.  It is thought that for all starting
-- numbers, the chains finish at the number 1.

-- For all starting numbers between 1 and 100, how many Collatz sequences have a
-- length greater than 15?

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz x
  | odd x = x:(collatz (x*3+1))
  | otherwise = x:collatz(div x 2)

numLongChains :: Int
numLongChains = length(filter (>15) (map length [collatz x | x <- [1..100]]))

numLongChains' :: Int
numLongChains' = length (filter isLong (map collatz [1..100]))
    where isLong xs = length xs > 15

numLongChains2 :: Int
numLongChains2 = length (filter (\xs -> length xs > 15) (map collatz [1..100]))

-- Standard functions implemented with fold

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) []

product' :: (Num a) => [a] -> a
-- product' xs = foldl (\acc x -> acc*x) 1 xs
product' = foldl (*) 1

head' :: [a] -> a
head' xs = foldr1 (\x _ -> x) xs

head2 :: [a] -> a
head2 = foldr1 (\x _ -> x)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x:acc else acc) []

last' :: [a] -> a
last' xs = foldl1 (\_ x -> x) xs

last2 :: [a] -> a
last2 = foldl1 (\_ x -> x)

-- How many elements does it take for the sum of the square roots of all natural
-- numbers to exceed 1000?

sumSqrt1000 :: Int
sumSqrt1000 = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- Simplify this expression using function composition
-- replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
-- replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]

-- Exercises

-- Sum the numbers between two inclusive values recursively, assuming a < b when the function is first called
-- Example: sumInts 0 1 = 1
--          sumInts 1 3 = 6
sumInts :: Int -> Int -> Int
sumInts a b
  | a > b = 0
  | otherwise = a + sumInts (a+1) b

-- Define a square function
sq :: Int -> Int
sq x = x*x

-- Sum the squares between two numbers. This function should be similar to the sumInts function
sumSquares :: Int -> Int -> Int
--sumSquares a b = foldl1 (+) (map (^2) [a..b])
sumSquares a b
  | a > b = 0
  | otherwise = sq a + sumSquares (a+1) b

-- Define a higher order sum function which accepts an (Int -> Int) function to apply to all integers between two values.
-- Again this should look similar to the sumInts and sumSquares functions
higherOrderSum :: (Int -> Int) -> Int -> Int -> Int
higherOrderSum intApplication a b
  | a > b = 0
  | otherwise = intApplication a + higherOrderSum intApplication (a+1) b

-- Define the square sum in terms of higherOrderSum
hoSumSquares :: Int -> Int -> Int
hoSumSquares = higherOrderSum sq

-- Define the sum between two values in terms of higherOrderSum
-- Note there is no parameter on the function definition
-- Try to use a lambda if possible
hoSumInts :: Int -> Int -> Int
hoSumInts = higherOrderSum (\x -> x)

-- Create a new higher order method which generalizes over the function provided by sumInts (That is, parameterize (+) :: Int -> Int -> Int) between a and b
-- This will give the ability to perform utilities such as the product of all squares (or any other Int -> Int function) between a and b
-- You will also need to generalize the base case
-- You can also define the function signature yourself, which leaves you free to define the parameters and their order
-- To be clear, your function will need to handle:
--  - A start value, a :: Int
--  - A end value, b :: Int
--  - A function to apply to each value, op :: Int -> Int
--  - A function to apply between each value, f :: Int -> Int -> Int
--  - A value to return in the base case when a > b, z :: Int
higherOrderSequenceApplication :: (Int -> Int -> Int) -> (Int -> Int) -> Int -> Int -> Int -> Int
higherOrderSequenceApplication binIntApp unIntApp a b z
  | a > b = z
  | otherwise = binIntApp (unIntApp a) (higherOrderSequenceApplication binIntApp unIntApp (a+1) b z)

-- Define a factorial method using the higherOrderSequenceAppliction
hoFactorial :: Int -> Int
hoFactorial b = higherOrderSequenceApplication (*) id 1 b 1
