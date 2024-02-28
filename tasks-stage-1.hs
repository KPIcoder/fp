import Data.List (find)

main = putStrLn "Hello World!"

sideEffectProduct :: Num a => (a -> a) -> a -> a -> a
sideEffectProduct f x y = f x * f y
sideEffect = prod 10

prod:: Num a => a -> a -> a
prod x y = x * y

factorial:: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n - 1)

ln :: [a] -> Int
ln [] = 0
ln (head : tail) = 1 + ln tail

customMap :: (a -> b) -> [a] -> [b]
customMap _ [] = []
customMap f (x: xs) = f x : customMap f xs

customFilter :: (a -> Bool) -> [a] -> [a]
customFilter _ [] = []
customFilter predicate (x:xs)
  | predicate x = x : customFilter predicate xs
  | otherwise = customFilter predicate xs

customZip :: [a] -> [b] -> [(a, b)]
customZip _ [] = []
customZip [] _ = []
customZip (x:xs) (y:ys) = (x, y) : customZip xs ys

indexify :: [a] -> [(Int, a)]
indexify = go 0
  where
    go _ [] = []
    go index (x: xs) = (index, x) : go (index + 1) xs

-- uni task to remove every n-th element 
removeEveryNth :: Int -> [a] -> [a]
removeEveryNth n xs = remover 1 xs
  where
    remover _ [] = []
    remover i (x : xs)
      | i `mod` n == 0 = remover (i + 1) xs
      | otherwise = x : remover (i + 1) xs

removeEveryNthPrelude :: Int -> [a] -> [a]
removeEveryNthPrelude n xs = map snd . filter (\(i, _) -> i `mod` n /= 0) $ zip [1..] xs


-- uni task to find the first Prime in the list
isPrime :: Int -> Bool
isPrime n = checkDivision 2
  where
    checkDivision divider
      | divider == n = True
      | n `mod` divider == 0 = False
      | otherwise = checkDivision (divider + 1)

findPrime :: [Int] -> Int
findPrime [] = error "No prime number in given array"
findPrime (x:xs)
  | isPrime x = x
  | otherwise = findPrime xs

findPrimePrelude :: [Int] -> Maybe Int
findPrimePrelude = find isPrime