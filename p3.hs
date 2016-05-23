elementAt :: [a] -> Int -> a
elementAt [] _ = error "error"
elementAt _ n | n <= 0 = error "error"
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n - 1)