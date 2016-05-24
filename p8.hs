compress :: Eq a => [a] -> [a]
compress list = compress' list []
    where compress' [] xs = reverse xs
          compress' (x:xs) [] = compress' xs [x]
          compress' (x:xs) (y:ys)
              | x == y = compress' xs (y:ys)
              | otherwise = compress' xs (x:y:ys)
