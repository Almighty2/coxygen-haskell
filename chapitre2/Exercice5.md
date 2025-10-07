-- Définition de l'aire d’un cercle
circleArea :: Float -> Float
circleArea r = pi * r * r

-- Maximum de trois entiers
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z = max x (max y z)

-- Programme principal avec des tests
main :: IO ()
main = do
  putStrLn $ "Aire d’un cercle de rayon 2.0 : " ++ show (circleArea 2.0)
  putStrLn $ "Aire d’un cercle de rayon 5.5 : " ++ show (circleArea 5.5)
  
  putStrLn $ "Maximum entre 3, 7 et 2 : " ++ show (maxOfThree 3 7 2)
  putStrLn $ "Maximum entre 10, 5 et 8 : " ++ show (maxOfThree 10 5 8)
  putStrLn $ "Maximum entre -1, -3 et -2 : " ++ show (maxOfThree (-1) (-3) (-2))
