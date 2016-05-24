isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = f == reverse b
    where len = length xs
          h = len `div` 2
          (f, b') = splitAt h xs
          b = drop (len `mod` 2) b'