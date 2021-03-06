import Data.List(group, partition)

-- 1

myLast :: [a] -> a
myLast [] = error "error"
myLast [x] = x
myLast (_:xs) = myLast xs


-- 2

myButLast :: [a] -> a
myButLast [] = error "error"
myButLast [x] = error "error"
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs


-- 3

elementAt :: [a] -> Int -> a
elementAt [] _ = error "error"
elementAt _ n | n <= 0 = error "error"
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n - 1)


-- 4

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs


-- 5

myReverse :: [a] -> [a]
myReverse xs = r xs []
    where r [] ns = ns
          r (y:ys) ns = r ys (y:ns)
          

-- 6

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = f == reverse b
    where len = length xs
          h = len `div` 2
          (f, b') = splitAt h xs
          b = drop (len `mod` 2) b'
          

-- 7

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concat $ map flatten xs


-- 8

compress :: Eq a => [a] -> [a]
compress list = compress' list []
    where compress' [] xs = reverse xs
          compress' (x:xs) [] = compress' xs [x]
          compress' (x:xs) (y:ys)
              | x == y = compress' xs (y:ys)
              | otherwise = compress' xs (x:y:ys)


-- 9

pack :: Eq a => [a] -> [[a]]
pack = foldr connect []
    where connect x [] = [[x]]
          connect x (y@(z:_):ys)
            | x == z =  (x:y):ys
            | otherwise = [x]:y:ys
                 

-- 10
  
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack


-- 11

data CountElement a = Single a | Multiple Int a deriving Show

encodeModified :: Eq a => [a] -> [CountElement a]
encodeModified = map m . pack
    where m [x] = Single x
          m xs = Multiple (length xs) (head xs)


-- 12

decodeModified :: [CountElement a] -> [a]
decodeModified = concat . map m
    where m (Single x) = [x]
          m (Multiple n x) = take n $ repeat x
          

-- 13

encodeDirect :: Eq a => [a] -> [CountElement a]
encodeDirect = foldr connect []
    where connect x [] = [Single x]
          connect x (Single y:ys)
            | x == y = Multiple 2 y : ys
            | otherwise = Single x : Single y : ys
          connect x (h@(Multiple n y) : ys)
            | x == y  = Multiple (n + 1) y : ys
            | otherwise = Single x : h : ys


-- 14

dupli :: [a] -> [a]
dupli = foldr (\i r -> i:i:r) []


-- 15

repli :: [a] -> Int -> [a]
repli xs n = concat $ map (take n . repeat) xs


-- 16

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = map snd $ filter (\x -> fst x `mod` n /= 0) $ zip [1..] xs


-- 17

split :: [a] -> Int -> ([a], [a])
split list n = sp list [] n'
    where n' = if n > length list then length list else n
          sp xs ys 0 = (ys, xs)
          sp (x:xs) ys n = sp xs (x:ys) (n - 1)
          
          
-- 18

slice :: [a] -> Int -> Int -> [a]
slice xs start end = drop (start - 1) $ take end xs


-- 19
rotate :: [a] -> Int -> [a]
rotate list n
  | n > 0 = drop n list ++ take n list
  | n < 0 = rotate list (length list + n)
  | otherwise = list

-- 20

removeAt :: Int -> [a] -> (a, [a])
removeAt n list 
  | n <= 0 || n > length list = error "error"
  | otherwise = (s, b)
    where (sf, sb) = splitAt n list
          s = last sf
          b = init sf ++ sb


-- 21

insertAt :: a -> [a] -> Int -> [a]
insertAt x list pos
  | pos < 0 = insertAt x list 0
  | pos > 1 + length list = insertAt x list (1 + length list)
  | otherwise = s ++ (x:b)
        where (s, b) = splitAt (pos - 1) list
        
        
-- 22

range :: Int -> Int -> [Int]
range x y = [x..y]


-- 31

isPrime :: Int -> Bool
isPrime n = all (/= 0) (map (n `mod`) (takeWhile (\x -> x * x <= n) [2..]))


-- 32

myGCD :: Int -> Int -> Int
myGCD x y = gcd (abs x) (abs y)
    where gcd m 0 = m
          gcd m n = case compare m n of
            GT -> gcd n (m `mod` n)
            LT -> gcd n m
            EQ -> m


-- 33

coprime :: Int -> Int -> Bool
coprime x y = myGCD x y == 1


-- 34

totient :: Int -> Int
totient n = length $ filter (coprime n) [1..n]


-- helpers

pfs :: Int -> Int -> [Int]
pfs _ 1 = []
pfs x n = if r == 0
         then x: pfs x d
         else pfs (x + 1) n
      where (d, r) = n `quotRem` x

-- 35

primeFactors :: Int -> [Int]
primeFactors = pfs 2


-- 36
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = map (\x -> (head x, length x)) . group . pfs 2


-- 37
phi :: Int -> Int
phi = floor . product . map (\(x, y) -> (fromIntegral x - 1) * (fromIntegral x) ** (fromIntegral y - 1)) . primeFactorsMult 


-- 39
primesR :: Int -> Int -> [Int]
primesR lower upper = filter notzeros [max 2 lower .. upper]
      where notzeros x = all (\p -> x == p || x `rem` p /= 0) primes
            primes = ps $ takeWhile (\x -> x * x <= upper) [2..upper]
            ps [] = []      
            ps (x:xs) = x : ps [y | y <- xs, y `rem` x /= 0]

-- 40
goldbach :: Int -> (Int, Int)
goldbach n = if isPrime half then (half, half) else find as (reverse bs)
      where half = round (fromIntegral n / 2)
            find (x:xs) (y:ys)
              | x + y == n = (x, y)
              | x + y > n = find (x:xs) ys
              | otherwise = find xs (y:ys)
            (as, bs) = partition (\x -> fromIntegral x <= half) primes
            primes = ps [2..n-2]
            ps [] = []
            ps (x:xs) = x : ps [y | y <- xs, y `rem` x /= 0]

-- 41
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList b e = map goldbach $ filter (\x -> x `rem` 2 == 0) [b..e]
