-- Fonction prenant trois entiers et retournant le maximum
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z =
    let
        maxXY = if x > y then x else y  -- maximum entre x et y
        maxXYZ = if maxXY > z then maxXY else z  -- maximum entre maxXY et z
    in
        maxXYZ

-- Tests
main :: IO ()
main = do
    print (maxOfThree 10 20 15)  -- 20
    print (maxOfThree 5 25 10)   -- 25
