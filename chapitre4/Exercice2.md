-- Fonction qui détermine si un jour est un weekend ou un jour de semaine
dayType :: String -> String
dayType day = case day of
    "Saturday" -> "It's a weekend!"
    "Sunday"   -> "It's a weekend!"
    "Monday"   -> "It's a weekday."
    "Tuesday"  -> "It's a weekday."
    "Wednesday"-> "It's a weekday."
    "Thursday" -> "It's a weekday."
    "Friday"   -> "It's a weekday."
    _          -> "Invalid day"  -- Cas pour toute autre entrée

-- Tests
main :: IO ()
main = do
    print (dayType "Saturday")  -- "It's a weekend!"
    print (dayType "Monday")    -- "It's a weekday."
    print (dayType "Funday")    -- "Invalid day"
