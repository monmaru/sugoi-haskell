solveRPN :: String -> Double
solveRPN = head . foldl foldingFuntion [] . words
    where foldingFuntion (x:y:ys) "*" = (y * x):ys
          foldingFuntion (x:y:ys) "+" = (y + x):ys
          foldingFuntion (x:y:ys) "-" = (y - x):ys
          foldingFuntion (x:y:ys) "/" = (y / x):ys
          foldingFuntion (x:y:ys) "^" = (y ** x):ys
          foldingFuntion (x:xs) "ln" = log x:xs
          foldingFuntion xs "sum" = [sum xs]
          foldingFuntion xs numberString = read numberString:xs
