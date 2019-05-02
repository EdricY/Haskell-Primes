
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

evalrpn :: String -> Int
evalrpn s = evalrpn2 (mywords s) []

  
evalrpn2 :: [String] -> [Int] -> Int
evalrpn2 [] ns = head ns
evalrpn2 (x:xs) ns
  | isdigit (head x) = evalrpn2 xs (xint:ns)
  | x == "+" = evalrpn2 xs ((n1+n2):ttns)
  | x == "*" = evalrpn2 xs ((n1*n2):ttns)
  where
    xint = read x :: Int
    tns = tail ns
    n1 = head ns
    n2 = head tns
    ttns = tail tns

infix2rpn :: String -> String
infix2rpn s = myunwords (parseExpr (mywords s))

parseExpr :: [String] -> [String]
parseExpr words = 
  term ++ (parseTerms rem)
  where
    term = parseTerm words
    rem = drop (length term) words

parseTerms :: [String] -> [String]
parseTerms [] = []
parseTerms (lookahead:words)
  | lookahead == "+" = term 
                        ++ ["+"]
                        ++ parseTerms rem
  | otherwise = []
  where
    term = parseTerm words
    rem = drop (length term) words

parseTerm :: [String] -> [String]
parseTerm words = 
  factor ++ (parseFactors rem)
  where
    factor = parseFactor words
    rem = drop (length factor) words

parseFactors :: [String] -> [String]
parseFactors [] = []
parseFactors (lookahead:words)
  | lookahead == "*" = factor 
                        ++ ["*"]
                        ++ parseFactors rem
  | otherwise = []
  where
    factor = parseFactor words
    rem = drop (length factor) words

parseFactor :: [String] -> [String]
parseFactor (lookahead:words)
  | isdigit (head lookahead) = [lookahead]
  | lookahead == "(" = ["?"] ++ (parseExpr words) ++ ["?"]
  -- two extra elements makes the previous call drop the parentheses
  | otherwise = []

myunwords :: [String] -> String
myunwords [x] = x
myunwords (x:xs)
  | x == "?" = myunwords xs
  | otherwise = x ++ " " ++ myunwords xs

isparen c = (c == '(' || c == ')')
isop    c = (c == '+' || c == '*')
isdigit c = (c >= '0' && c <= '9')

-- from Dr. Denning
mywords :: String -> [String]
mywords s = parser [] s
  where
    -- readint reads digits until non-digit
    -- returns ("digit","remainder of string")
    readint "" = ("","")
    readint (h:t)
      | (isdigit h) = (h:(fst (readint t)), snd (readint t))
      | otherwise  = ("", h:t)
    parser l "" = l
    parser l (h:t)
      | (isdigit h) = parser (l ++ [fst (readint (h:t))])
                              (snd (readint (h:t)))
      | (isop h)    = parser (l ++ [h:""]) t
      | (isparen h) = parser (l ++ [h:""]) t
      | h == ' '    = parser l t