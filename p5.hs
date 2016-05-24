myReverse :: [a] -> [a]
myReverse xs = r xs []
    where r [] ns = ns
          r (y:ys) ns = r ys (y:ns)