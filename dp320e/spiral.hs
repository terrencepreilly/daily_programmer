import Data.List ( transpose
                 , intercalate
                 )

digits x = succ . floor $ (log x) / (log 10)

biggest :: String -> Int
biggest x = digits . (^2) $ read x :: Int

display :: Show t => Int -> t -> [Char]
display padding i = take remaining (repeat ' ') ++ x
    where
        x = show i
        remaining = padding - length x

rotate :: [[a]] -> [[a]]
rotate = rev' . transpose
    where
        rev' x = map reverse x

stitch :: Integral a => a -> [[a]] -> [[a]]
stitch x xs = map apply $ zip as xs
    where
        apply (b,bs) = b : bs
        as = take (length xs) $ iterate pred x


spiral :: Int -> [[Int]] -> [[Int]]
spiral highest xs
    | nextHighest < 0 = xs
    | otherwise = spiral nextHighest . rotate $ stitch highest xs
        where
            nextHighest = highest - length xs

printSpiral :: Show a => Int -> [[a]] -> [Char]
printSpiral h = unlines . map ((intercalate " ") . (map (display h)))


example = [[1, 2], [3, 4]]

m :: String -> String
m x = printSpiral (biggest x) $ spiral (pred d) [[d]]
    where
        h = read x :: Int
        d = (h ^ 2)

main :: IO ()
main = interact m
