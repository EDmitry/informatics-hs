solve :: [Integer] -> Integer
solve (n:m:a:xs) = hor * vert
  where hor = n `div` a + if n `mod` a == 0 then 0 else 1
        vert = m `div` a + if m `mod` a == 0 then 0 else 1
main = getLine >>= putStrLn . show . solve . map read . words
