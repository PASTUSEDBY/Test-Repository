{-
    Check whether a number is a magic number. A number is a magic number when the cumulative sum of the digits of a number
    is a single digit and that digit equals to 1.
    
    Example - 28
            = 2 + 8 = 10
            = 1 + 0 = 1
            = Magic Number
-}

divide :: Int -> [Int]
divide x
    | x <= 0 = []
    | otherwise = again
    where
        let n = x `mod` 10
        again = n : divide $ x `div` 10
        

magic :: Int -> Bool
magic x
    | x < 10 = check
    | otherwise = again
    where
        let a = divide x
        again = magic a
        check = if x == 1
                    then True
                    else False
                    
 main = do
     putStrLn "Enter a number: "
     n <- getLine
     let x = read n :: Int
     if magic x
         then do
             putStrLn "Magic Number"
         else do
             putStrLn "Not Magic Number"
