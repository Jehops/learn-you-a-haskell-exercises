{-
 - For this exercise, we are dealing with a type for colours of the rainbow
 - The typeclass is defined here, and note its Canadian spelling.
 - For more information on how this is done, look ahead to:
 - http://learnyouahaskell.com/making-our-own-types-and-typeclasses
 -
 - Have a play with the Colour in ghci, try the succ and pred functions and so on.
 -}
data Colour = Red | Orange | Yellow | Green | Blue | Indigo | Violet
    deriving (Eq, Ord, Show, Bounded, Enum)

{-
 - Again, you should be able to write these functions in one line,
 - using the information from the chapter http://learnyouahaskell.com/types-and-typeclasses
 - and the chapter before
 -}

{-
 - The Colour typeclass is of type Ord
 - What is the "first" (or least) colour
 -}
firstColour :: Colour
firstColour = minBound :: Colour

--List the colours in reverse order
reverseColourOrder :: [Colour]
reverseColourOrder = reverse [minBound :: Colour .. maxBound]

{-
 - Mix two colours together, to produce the average value of the two.
 - Example: paintMix Orange Green = Yellow
 - If necessary, favour the "higher" value when computing the average.
 - For example: paintMix Green Violet = Indigo
 - Hint: Integer division can be performed with the quot function: quot 7 2 = 3
 -}
paintMix :: Colour -> Colour -> Colour
paintMix c1 c2
  | odd colourSum = toEnum (quot (colourSum + 1) 2) :: Colour
  | otherwise = toEnum (quot colourSum 2) :: Colour
  where colourSum = fromEnum c1 + fromEnum c2

paintMix2 :: Colour -> Colour -> Colour
paintMix2 c1 c2 =
  let colourSum = fromEnum c1 + fromEnum c2
  in toEnum (quot (colourSum + 1) 2) :: Colour

paintMix3 :: Colour -> Colour -> Colour
paintMix3 c1 c2 = toEnum (quot (fromEnum c1 + fromEnum c2 + 1) 2) :: Colour