-- Fonction qui prend une condition météo et retourne un message descriptif
weatherReport :: String -> String
weatherReport condition = case condition of
    "sunny"  -> "It's a bright and beautiful day!"
    "rainy"  -> "Don't forget your umbrella!"
    "cloudy" -> "A bit gloomy, but no rain yet!"
    _        -> "Weather unknown"  -- Cas par défaut pour toute autre entrée

-- Tests
main :: IO ()
main = do
    print (weatherReport "sunny")   -- "It's a bright and beautiful day!"
    print (weatherReport "rainy")   -- "Don't forget your umbrella!"
    print (weatherReport "cloudy")  -- "A bit gloomy, but no rain yet!"
    print (weatherReport "foggy")   -- "Weather unknown"
