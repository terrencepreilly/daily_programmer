onesPlace :: Int -> String
onesPlace x = [ "zero"
              , "one"
              , "two"
              , "three"
              , "four"
              , "five"
              , "six"
              , "seven"
              , "eight"
              , "nine"
              ] !! x

teens :: Int -> String
teens x = [ "ten"
          , "eleven"
          , "twelve"
          , "thirteen"
          , "fourteen"
          , "fifteen"
          , "sixteen"
          , "seventeen"
          , "eighteen"
          , "nineteen"
          ] !! mod x 10

tensPlace :: Int  -> String
tensPlace x
    | 0 < x && x < 10 = onesPlace x
    | x `div` 10 == 1 = teens x
    | otherwise = if ones > 0 then tens ++ "-" ++ onesPlace ones else tens
    where
        tens  = [ "twenty"
                , "thirty"
                , "fourty"
                , "fifty"
                , "sixty"
                , "seventy"
                , "eighty"
                , "ninety"
                ] !! ((div x 10) - 2)
        ones = mod x 10



hundredsPlace :: Int -> String
hundredsPlace x
    | x < 100 = tensPlace x
    | otherwise = hundreds ++ tens
        where
            hundreds = (onesPlace (div x 100)) ++ " hundred"
            tens = if ten == 0 then "" else " " ++ (tensPlace ten)
                where
                    ten = mod x 100

regularPlace :: Int -> String -> (Int -> String) -> Int -> String
regularPlace divisor repr previous x
    | x < divisor = previous x
    | otherwise = thisPlace ++ previousPlace
        where
            thisPlace = hundredsPlace (div x divisor) ++ " " ++ repr
            previousPlace = if prev == 0 then "" else " " ++ previous prev
                where
                    prev = mod x divisor

thousandsPlace :: Int -> String
thousandsPlace = regularPlace (10^3) "thousand" hundredsPlace

millionsPlace :: Int -> String
millionsPlace = regularPlace (10^6) "million" thousandsPlace

billionsPlace :: Int -> String
billionsPlace = regularPlace (10^9) "billion" millionsPlace

trillionsPlace :: Int -> String
trillionsPlace = regularPlace (10^12) "trillion" billionsPlace

checkAmount :: Float -> String
checkAmount x
    | fractionalPart > 0 = trillionsPlace integerPart ++ " dollars and " ++ hundredsPlace fractionalPart ++ " cents"
    | otherwise = trillionsPlace integerPart ++ " dollars and zero cents"
    where
        integerPart = floor x
        fractionalPart = round (100 * (x - (fromIntegral integerPart)))

splitAmount :: Float -> (Int, Int)
splitAmount x = (i, f)
    where
        i = floor x
        f = round (100 * (x - (fromIntegral i)))
