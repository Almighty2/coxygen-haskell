-- Signature de type : prend un Double (Fahrenheit), retourne un Double (Celsius)
fToC :: Double -> Double
fToC f = (f - 32) * 5 / 9
-- Applique directement la formule de conversion

-- Fonction main pour tester avec une valeur donnée
main :: IO ()
main = do
    let fahrenheit = 98.6
    let celsius = fToC fahrenheit
    putStrLn ("La température " ++ show fahrenheit ++ "°F équivaut à " ++ show celsius ++ "°C.")
