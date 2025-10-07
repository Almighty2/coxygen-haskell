-- Signature de type :
-- applyTwice prend une fonction (a -> a), une valeur de type a,
-- et retourne le résultat de l'application deux fois de cette fonction
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
-- f est appliquée une première fois sur x, puis encore une fois sur le résultat

-- Exemple : doubler deux fois un nombre
double :: Int -> Int
double x = x * 2

-- Fonction main pour tester
main :: IO ()
main = do
    let val = 3
    let result = applyTwice double val
    putStrLn ("Appliquer double deux fois sur " ++ show val ++ " donne : " ++ show result)
