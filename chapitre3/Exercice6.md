-- Déclaration de la fonction isLeapYear prenant une année (Int) et retournant Bool
isLeapYear :: Int -> Bool
isLeapYear year =
    if year `mod` 400 == 0 then
        True                            -- Divisible par 400 → bissextile
    else if year `mod` 100 == 0 then
        False                           -- Divisible par 100 mais pas 400 → pas bissextile
    else if year `mod` 4 == 0 then
        True                            -- Divisible par 4 → bissextile
    else
        False                           -- Sinon → pas bissextile

-- Tests
main :: IO ()
main = do
    print (isLeapYear 2000) -- True (bissextile)
    print (isLeapYear 1900) -- False (pas bissextile)
    print (isLeapYear 2024) -- True (bissextile)
