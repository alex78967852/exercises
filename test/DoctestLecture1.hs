module Main (main) wheres
Prelude> take 5 ("kek :" ++ "Hello") ++ "world"
"kek :world"


module SumSquare where
    sumSquare :: Int -> Int -> Int
    sumSquare x y = (x + y)^2


module LastDigit where
    lastDigit :: Integer -> Integer
    lastDigit n
        | (n < 0) = ((-1) * n ) `mod` 10
        | otherwise = n `mod` 10



module MinMax where 
    minMax :: Int -> Int -> Int -> Int 
    minMax x y z = 
          let  minxyz = min x (min y z)
               maxxyz = max x (max y z)
          in   maxxyz - minxyz
          
          
          module SubString where
    subString :: Int -> Int -> String -> String
    subString x y text =       
     
        if length (text) < x
        then "Too short string"
        else   drop x (take y text)    
            
            
            module SumStr where
    sumStr :: Int -> Int -> Int-> Int
    sumStr x y z = x + y +z
    
    
    module Count where 
  count :: Int-> [Int] -> Int
  count n list = go 0 list
    where 
      go :: Int -> [Int] -> Int
      go result l =
        if null l
        then result
        else if head l < n
             then go ( result + 1) (tail l)
             else go result        (tail l)


import Test.DocTest (doctest)


main :: IO ()
main = doctest
    [ "src/Lecture1.hs"
    ]
