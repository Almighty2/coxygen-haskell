-- Déclaration de la fonction avec son type :
-- checkNumber prend un entier (Int) et renvoie une chaîne de caractères (String)
checkNumber :: Int -> String
checkNumber n =
    -- Utilisation de if-then-else pour vérifier la valeur de n
    if n > 0 then
        "Positive"   -- Si n est supérieur à 0
    else if n < 0 then
        "Negative"   -- Si n est inférieur à 0
    else
        "Zero"       -- Sinon (donc n == 0)

-- Tests de la fonction
main :: IO ()
main = do
    print (checkNumber 5)    -- Devrait afficher "Positive"
    print (checkNumber (-3)) -- Devrait afficher "Negative"
    print (checkNumber 0)    -- Devrait afficher "Zero"
