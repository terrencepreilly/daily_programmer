inRange :: [a] -> Int -> Bool
inRange xs i = i >= 0 && i < length xs

_zero :: Int -> [Int] -> Int
_zero i xs
    | not (inRange xs i)    = -1
    | i == 0 && xs !! i > 0 = -1
    | xs !! i < 0           = _zero (i+1) xs
    | otherwise             = i

zero :: [Int] -> Int
zero = _zero 0

_balance :: [Int] -> Int -> Int -> Bool
_balance xs i j
    | not (inRange xs i) = False
    | curr > 0           = _balance xs (i-1) j
    | curr < 0           = _balance xs i (j+1)
    | otherwise          = True
    where
        curr = (xs !! i) + (xs !! j)

balance :: [Int] -> Bool
balance xs = let z = zero xs in _balance xs z z
