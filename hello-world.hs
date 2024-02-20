main = putStrLn "Hello World!"

sideEffectProduct :: Num a => (a -> a) -> a -> a -> a
sideEffectProduct f x y = f x * f y
sideEffect = prod 10

prod:: Num a => a -> a -> a
prod x y = x * y

factorial:: Int -> Int
factorial 1 = 1
factorial n = n * factorial(n - 1)

ln :: [a] -> Int
ln [] = 0
ln (head : tail) = 1 + ln tail

-- removeEveryNth :: Int -> [a] -> [a]
-- removeEveryNth _ [] = [] -- If the list is empty, return an empty list
-- removeEveryNth n xs
--   | n <= 0    = xs -- If n is less than or equal to 0, return the list as is
--   | otherwise = remover 1 xs -- Start with position 1
--   where
--     remover _ [] = [] -- Base case for recursion
--     remover pos (y:ys)
--       | pos `mod` n == 0 = remover (pos + 1) ys -- Skip this element
--       | otherwise        = y : remover (pos + 1) ys -- Keep this element

-- -- Example usage:
-- -- removeEveryNth 3 [1,2,3,4,5,6,7,8,9]
-- -- Output: [1,2,4,5,7,8]
