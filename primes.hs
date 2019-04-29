main = print (firstn 1)

prods :: Int -> [Int]
prods n = [n*n, (n*n)+n..]

mix :: [Int] -> [Int] -> [Int]
mix [] bs = bs
mix as [] = as
mix (a:as) (b:bs)
  | a < b = a:(mix as (b:bs))
  | b < a = b:(mix (a:as) bs)
  | otherwise = a:(mix as bs)

sieve :: [Int] -> [Int] -> [Int]
-- sieve potentialPrimes composites -> primes
sieve [] cs = []
sieve (x:xs) [] = x:(sieve xs (prods x))
sieve (x:xs) (c:cs)
  | x == c = sieve xs cs
  | otherwise = x:sieve xs (mix (prods x) (c:cs))

firstn :: Int -> [Int]
firstn n = take n (sieve [2,3..] [])

primesto :: Int -> [Int]
primesto p = sieve [2..p] []


mergesort :: [Int] -> [Int]
mergesort a = mergesort2 (length a) a

mergesort2 :: Int -> [Int] -> [Int]
mergesort2 len a
  | len <= 1 = a
  | otherwise =
    let mid = quot len 2 in
    let (front, back) = splitAt mid a in
      merge
        (mergesort2 mid front)
        (mergesort2 (len - mid) back)

merge :: [Int] -> [Int] -> [Int]
merge [] bs = bs
merge as [] = as
merge (a:as) (b:bs)
  | a < b     = a:(merge as (b:bs))
  | otherwise = b:(merge (a:as) bs)


quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort [x] = [x]
quicksort (h:t) =
  let (lower, upper) = partition h t in
  let s_lower = quicksort lower in
  let s_upper = quicksort upper in
  s_lower ++ h:s_upper

getPivot :: [Int] -> Int
getPivot lst = head lst

partition :: Int -> [Int] -> ([Int], [Int])
partition pivot xs =
  (lower, upper)
    where
      lower = lowerPartition pivot xs
      upper = upperPartition pivot xs

lowerPartition :: Int -> [Int] -> [Int]
lowerPartition pivot xs =
  filter (< pivot) xs
  
upperPartition :: Int -> [Int] -> [Int]
upperPartition pivot xs =
  filter (>= pivot) xs
