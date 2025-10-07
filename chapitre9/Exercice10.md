{-# LANGUAGE FlexibleInstances #-}

module Main where

-- Définition du type récursif BST a (Binary Search Tree)
data BST a = EmptyBST | NodeBST a (BST a) (BST a) deriving (Show, Eq)

-- | Crée un arbre vide
emptyBST :: BST a
emptyBST = EmptyBST

-- | Crée un nœud avec une valeur et deux sous-arbres
nodeBST :: a -> BST a -> BST a -> BST a
nodeBST = NodeBST

-- | Insère un élément dans l'arbre en respectant la propriété BST
insertBST :: (Ord a) => a -> BST a -> BST a
insertBST x EmptyBST = NodeBST x EmptyBST EmptyBST
insertBST x (NodeBST y left right)
    | x <= y    = NodeBST y (insertBST x left) right
    | otherwise = NodeBST y left (insertBST x right)

-- | Convertit une liste en BST
fromListBST :: (Ord a) => [a] -> BST a
fromListBST = foldr insertBST EmptyBST

-- | Vérifie si un arbre est empty
isEmptyBST :: BST a -> Bool
isEmptyBST EmptyBST = True
isEmptyBST _ = False

-- | Recherche un élément dans l'arbre
searchBST :: (Ord a) => a -> BST a -> Bool
searchBST _ EmptyBST = False
searchBST x (NodeBST y left right)
    | x == y    = True
    | x < y     = searchBST x left
    | otherwise = searchBST x right

-- | Parcours en ordre (gauche, racine, droite)
inOrder :: BST a -> [a]
inOrder EmptyBST = []
inOrder (NodeBST x left right) = inOrder left ++ [x] ++ inOrder right

-- | Parcours pré-ordre (racine, gauche, droite)
preOrder :: BST a -> [a]
preOrder EmptyBST = []
preOrder (NodeBST x left right) = [x] ++ preOrder left ++ preOrder right

-- | Parcours post-ordre (gauche, droite, racine)
postOrder :: BST a -> [a]
postOrder EmptyBST = []
postOrder (NodeBST x left right) = postOrder left ++ postOrder right ++ [x]

-- | Hauteur de l'arbre
heightBST :: BST a -> Int
heightBST EmptyBST = 0
heightBST (NodeBST _ left right) = 1 + max (heightBST left) (heightBST right)

-- | Nombre de nœuds dans l'arbre
sizeBST :: BST a -> Int
sizeBST EmptyBST = 0
sizeBST (NodeBST _ left right) = 1 + sizeBST left + sizeBST right

-- | Vérifie si l'arbre est un BST valide
isValidBST :: (Ord a) => BST a -> Bool
isValidBST EmptyBST = True
isValidBST (NodeBST x left right) =
    all (<= x) (inOrder left) &&
    all (> x) (inOrder right) &&
    isValidBST left &&
    isValidBST right

-- | Trouve l'élément minimum dans l'arbre
minBST :: (Ord a) => BST a -> Maybe a
minBST EmptyBST = Nothing
minBST (NodeBST x EmptyBST _) = Just x
minBST (NodeBST _ left _) = minBST left

-- | Trouve l'élément maximum dans l'arbre
maxBST :: (Ord a) => BST a -> Maybe a
maxBST EmptyBST = Nothing
maxBST (NodeBST x _ EmptyBST) = Just x
maxBST (NodeBST _ _ right) = maxBST right

-- | Supprime un élément de l'arbre (version corrigée)
deleteBST :: (Ord a) => a -> BST a -> BST a
deleteBST _ EmptyBST = EmptyBST
deleteBST x (NodeBST y left right)
    | x < y     = NodeBST y (deleteBST x left) right
    | x > y     = NodeBST y left (deleteBST x right)
    | otherwise = case (left, right) of
        (EmptyBST, EmptyBST) -> EmptyBST
        (EmptyBST, _) -> right
        (_, EmptyBST) -> left
        _ -> case minBST right of
            Just minVal -> NodeBST minVal left (deleteBST minVal right)
            Nothing -> EmptyBST  -- Ce cas ne devrait jamais arriver pour un BST valide

-- Fonction main pour tester le BST
main :: IO ()
main = do
    putStrLn "=== Tests du Binary Search Tree ==="
    
    -- Création d'un BST à partir d'une liste
    let bst1 :: BST Int
        bst1 = fromListBST [5, 3, 7, 2, 4, 6, 8]
        
    let bst2 :: BST String
        bst2 = fromListBST ["banana", "apple", "cherry", "date"]
    
    putStrLn $ "BST Int: " ++ show bst1
    putStrLn $ "BST String: " ++ show bst2
    
    -- Tests de recherche
    putStrLn "\n=== Tests de recherche ==="
    putStrLn $ "searchBST 5 dans [5,3,7,2,4,6,8]: " ++ show (searchBST 5 bst1)
    putStrLn $ "searchBST 9 dans [5,3,7,2,4,6,8]: " ++ show (searchBST 9 bst1)
    putStrLn $ "searchBST \"apple\" dans [\"banana\",\"apple\",\"cherry\",\"date\"]: " ++ 
               show (searchBST "apple" bst2)
    
    -- Tests de parcours
    putStrLn "\n=== Tests de parcours ==="
    putStrLn $ "inOrder: " ++ show (inOrder bst1)
    putStrLn $ "preOrder: " ++ show (preOrder bst1)
    putStrLn $ "postOrder: " ++ show (postOrder bst1)
    
    -- Tests des propriétés
    putStrLn "\n=== Tests des propriétés ==="
    putStrLn $ "heightBST: " ++ show (heightBST bst1)
    putStrLn $ "sizeBST: " ++ show (sizeBST bst1)
    putStrLn $ "isEmptyBST: " ++ show (isEmptyBST emptyBST)
    putStrLn $ "isValidBST: " ++ show (isValidBST bst1)
    
    -- Tests min/max
    putStrLn "\n=== Tests min/max ==="
    putStrLn $ "minBST: " ++ show (minBST bst1)
    putStrLn $ "maxBST: " ++ show (maxBST bst1)
    
    -- Test d'insertion supplémentaire
    putStrLn "\n=== Test d'insertion ==="
    let bst3 = insertBST 1 bst1
    putStrLn $ "Après insertion de 1: " ++ show (inOrder bst3)
    
    -- Test de suppression
    putStrLn "\n=== Test de suppression ==="
    let bst4 = deleteBST 5 bst1
    putStrLn $ "Après suppression de 5: " ++ show (inOrder bst4)