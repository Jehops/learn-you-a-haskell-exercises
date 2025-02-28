-- Find the penultimate element in list l
penultimate :: [a] -> a
penultimate l = last (init l)

-- Find the element at index k in list l
findk :: Int -> [a] -> a
findk k l = head (drop k l)

-- Determine if list l is a palindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome l
  | head l == last l = isPalindrome (init (tail l))
  | otherwise = False

isPalindrome2 :: Eq a => [a] -> Bool
isPalindrome2 l
   | length l == 0 = True
   | length l == 1 = True
   | head l == last l = isPalindrome2 (init (tail l))
   | otherwise = False

{-
 - Duplicate the elements in list xs, for example "duplicate [1,2,3]" would give the list [1,1,2,2,3,3]
 - Hint: The "concat [l]" function flattens a list of lists into a single list. 
 - (You can see the function definition by typing ":t concat" into the interpreter. Perhaps try this with other variables and functions)
 -
 - For example: concat [[1,2,3],[3,4,5]] returns [1,2,3,3,4,5]
 -}
duplicate :: [a] -> [a]
duplicate xs = concat [[x,x] | x <- xs]

{-
 - Imitate the functinality of zip
 - The function "min x y" returns the lower of values x and y
 - For example "ziplike [1,2,3] ['a', 'b', 'c', 'd']" returns [(1,'a'), (2, 'b'), (3, 'c')]
 -}
ziplike :: [a] -> [b] -> [(a,b)]
ziplike [] _ = []
ziplike _ [] = []
ziplike (x:xs) (y:ys) = (x,y) : ziplike xs ys

-- Split a list l at element k into a tuple: The first part up to and including k, the second part after k
-- For example "splitAtIndex 3 [1,1,1,2,2,2]" returns ([1,1,1],[2,2,2])
splitAtIndex :: Int -> [a] -> ([a],[a])
splitAtIndex _ [] = ([],[])
splitAtIndex k l = (take k l,drop k l)

-- Drop the element at index k in list l
-- For example "dropK 3 [0,0,0,1,0,0,0]" returns [0,0,0,0,0,0]
dropK :: Int -> [a] -> [a]
dropK k l = take k l ++ drop (k+1) l

-- Extract elements between ith and kth element in list l. Including i, but not k
-- For example, "slice 3 6 [0,0,0,1,2,3,0,0,0]" returns [1,2,3]
slice :: Int -> Int -> [a] -> [a]
slice i k l = take (k-i) (drop i l)

-- Insert element x in list l at index k
-- For example, "insertElem 2 5 [0,0,0,0,0,0]" returns [0,0,0,0,0,2,0]
insertElem :: a -> Int -> [a] -> [a]
insertElem x k l = take k l ++ [x] ++ drop k l

-- Rotate list l n places left.
-- For example, "rotate 2 [1,2,3,4,5]" gives [3,4,5,1,2]
rotate :: Int -> [a] -> [a]
rotate n l = drop n l ++ take n l
