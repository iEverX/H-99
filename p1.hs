myLast :: [a] -> a
myLast [] = error "error"
myLast [x] = x
myLast (_:xs) = myLast xs