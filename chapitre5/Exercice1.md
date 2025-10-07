-- Applique f trois fois à x : f(f(f(x)))
applyThrice :: (Int -> Int) -> Int -> Int
applyThrice f x = f (f (f x))

-- Tests
main :: IO ()
main = do
    -- Exemple avec f = (+1), 3 fois ajouter 1 à 5 → 8
    print (applyThrice (+1) 5)   -- 8
    -- Exemple avec f = (*2), 3 fois multiplier par 2 à 2 → 16
    print (applyThrice (*2) 2)   -- 16
