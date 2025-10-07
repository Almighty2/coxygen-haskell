-- Applique la fonction f deux fois à chaque élément de la liste
transformList :: (a -> a) -> [a] -> [a]
transformList f xs = map (f . f) xs

-- Tests
main :: IO ()
main = do
    -- Exemple avec (+1), deux fois (+1) = (+2)
    print (transformList (+1) [1,2,3])  -- [3,4,5]
    
    -- Exemple avec (*2), deux fois (*2) = (*4)
    print (transformList (*2) [1,2,3])  -- [4,8,12]
