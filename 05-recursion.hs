-- Examples from the Chapter

myMaximum :: (Ord a) => [a] -> a
myMaximum [] = undefined
myMaximum [x] = x
myMaximum (x:xs)
  | x > maxTail = x
  | x < maxTail = maxTail
  where maxTail = myMaximum xs

myMaximum2 :: (Ord a) => [a] -> a
myMaximum2 [] = error "maximum of empty list"
myMaximum2 [x] = x
myMaximum2 (x:xs) = max x (maximum xs)

myReplicate :: (Num i, Ord i) => i -> a -> [a]
myReplicate n x
  | n <= 0 = []
  | otherwise = x:myReplicate (n-1) x

myTake :: (Num i, Ord i) => i -> [a] -> [a]
myTake n (x:xs)
  | n <= 0 = []
  | null (x:xs) = []
  | otherwise = x:myTake (n-1) xs

myTake2 :: (Num i, Ord i) => i -> [a] -> [a]
myTake2 n _
  | n <= 0 = []
myTake2 _ [] = []
myTake2 n (x:xs) = x:myTake2 (n-1) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

myRepeat :: a -> [a]
myRepeat x = x:myRepeat x

myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y):myZip xs ys

myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys)
  | x == y = True
  | otherwise = myElem x ys

myQS :: (Ord a) => [a] -> [a]
myQS [] = []
myQS (x:xs) =
  let smallerSorted = myQS [a | a <- xs, a <= x]
      biggerSorted = myQS [a | a <- xs, a > x]
  in  smallerSorted ++ [x] ++ biggerSorted

-- Raise x to the power y, using recursion
-- For example, power 5 2 = 25
--power :: Int -> Int -> Int
--power x y = undefined
myPower :: Int -> Int -> Int
myPower _ 0 = 1
myPower x y = x * myPower x (y-1)

-- create a list of length n of the fibbonaci sequence in reverse order
-- examples: fib 0 = [0]
-- 	     fib 1 = [1, 0]
--	     fib 10 = [55,34,21,13,8,5,3,2,1,1,0]
-- try to use a where clause
--fib :: (Num a, Eq a) => a -> [a]
--fib 0 = [0]
--fib 1 = [0,1]

fib :: (Integral a) => a -> [a]
fib n
  | n==0 = [0]
  | n==1 = 1:(fib 0)
  | otherwise = (sum (take 2 fibn1)):fibn1
  where fibn1 = fib (n-1)

-- This is not recursive, but have a go anyway.
-- Create a function which takes two parameters, a number and a step
-- The result is the sign of the original number reversed, and the step added to the absolute value
-- Confused? Some examples: stepReverseSign 6 2 = -8
--			    stepReverseSign -3 1 = 4
--			    stepReverseSign 1 2 = -3
--stepReverseSign :: (Fractional a, Ord a) => a -> a -> a
--stepReverseSign a = undefined
stepReverseSign :: (Fractional a, Ord a) => a -> a -> a
stepReverseSign x y = (abs x + y) * signum x * (-1)

{- Lets calculate pi.
 - The Leibniz formula for pi (http://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80)
 - Can be defined as pi = (4/1) - (4/3) + (4/5) - (4/7) ....
 - We can create a function, where given a certain tolerance, we can recursively calculate
 - Pi to within that tolerance.
 - Lets create two functions, piCalc, and piCalc', the latter we will recursively call
 - until our pi calculation is within the tolerance

 - The piCalc function is defined as:
 - piCalc :: (Fractional a, Integral b, Ord a) => a -> (a, b)

 - Given a tolerance, say, 0.001, it will return a tuple.
 - fst is pi to an accuracy of the tolerance, 0.001 in this case
 - snd is the number of recursive steps taken to calculate it, after all this chapter is about recursion!
 - Example: piCalc 0.001 = (3.1420924036835256,2000)

 - The piCalc' function is defined as
 - piCalc' :: (Ord a, Fractional a, Integral b) => a -> a -> a -> b -> (a, b)
 - Lots of parameters!
 - The first parameter is the current denominator from the Leibniz formula
 - The next is our calculation of pi from our previous attempt
 - The next is the tolerance
 - The final parameter is the number of times this function has been called (ie, we add one every time we recurse
 - Example piCalc' 1 0.0 0.001 0 = (3.1420924036835256,2000)
 -
 - Feel free to change the parameter order, what parameters you need etc in order to get this to work for you,
 - But, of course the output of piCalc should remain as (pi, count)
 -
 - You may find the stepReverseSign function handy
 -}

--piCalc' :: (Ord a, Fractional a, Integral b) => a -> a -> a -> b -> (a, b)
--piCalc' w x y z = undefined

piCalc :: (Ord a, Floating a, Integral b) => a -> (a, b)
piCalc a = piCalc' 1 0.0 a 0

piCalc' :: (Ord a, Floating a, Integral b) => a -> a -> a -> b -> (a,b)
piCalc' d p t n
  | abs (4/d) <= t = (p,n)
  | otherwise = piCalc' (stepReverseSign d 2) (p+(4/d)) t (n+1)

-- piCalc :: (Fractional a, Integral b, Ord a) => a -> (a, b)
-- piCalc a = piCalc' 1 0 a 0

-- piCalc' :: (Ord a, Fractional a, Integral b) => a -> a -> a -> b -> (a, b)
-- piCalc' w x y z
--   | abs(4/w) < y = (x, z)
--   | otherwise = piCalc' (stepReverseSign w 2) (x + (4/w)) y (z+1)
