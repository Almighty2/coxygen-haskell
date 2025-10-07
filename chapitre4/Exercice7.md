-- Fonction qui retourne un tuple avec le 1er et 3e élément de la liste
-- Si la liste est trop courte, renvoie Nothing
firstAndThird :: [a] -> Maybe (a, a)
firstAndThird (x:_:z:_) = Just (x, z)  -- On récupère le 1er et 3e élément
firstAndThird _         = Nothing      -- Sinon (liste trop courte), on retourne Nothing

-- Tests
main :: IO ()
main = do
    print (firstAndThird [10, 20, 30, 40]) -- Just (10,30)
    print (firstAndThird [1, 2])            -- Nothing (pas assez d'éléments)
    print (firstAndThird ["a", "b", "c"])  -- Just ("a","c")
