myButLast :: [a] -> a
myButLast [] = error "error"
myButLast [x] = error "error"
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs