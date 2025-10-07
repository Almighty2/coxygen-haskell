{-# LANGUAGE FlexibleInstances #-}

module Main where

-- Définition du type récursif Sequence a
-- Représente une séquence linéaire de nœuds
data Sequence a = Empty | Node a (Sequence a) deriving (Show, Eq)

-- | Crée une séquence vide
empty :: Sequence a
empty = Empty

-- | Convertit une liste en Sequence
fromList :: [a] -> Sequence a
fromList [] = Empty
fromList (x:xs) = Node x (fromList xs)

-- | Vérifie si un élément est présent dans la séquence
-- Utilise l'égalité (==) pour comparer les éléments
elemSeq :: (Eq a) => a -> Sequence a -> Bool
elemSeq _ Empty = False  -- Cas de base : séquence vide, élément non trouvé
elemSeq x (Node y next)
    | x == y    = True   -- Élément trouvé
    | otherwise = elemSeq x next  -- Recherche récursive dans le reste

-- Version alternative avec pattern matching plus concis
elemSeq' :: (Eq a) => a -> Sequence a -> Bool
elemSeq' x Empty = False
elemSeq' x (Node y next) = x == y || elemSeq' x next

-- Version utilisant foldl'
elemSeqFold :: (Eq a) => a -> Sequence a -> Bool
elemSeqFold x seq = foldl' (\acc y -> acc || x == y) False seq

-- Version utilisant foldr'
elemSeqFoldR :: (Eq a) => a -> Sequence a -> Bool
elemSeqFoldR x seq = foldr' (\y acc -> x == y || acc) False seq

-- Fonctions auxiliaires (définies précédemment)
foldl' :: (b -> a -> b) -> b -> Sequence a -> b
foldl' _ acc Empty = acc
foldl' f acc (Node x next) = foldl' f (f acc x) next

foldr' :: (a -> b -> b) -> b -> Sequence a -> b
foldr' _ acc Empty = acc
foldr' f acc (Node x next) = f x (foldr' f acc next)

-- Fonction main pour tester elemSeq
main :: IO ()
main = do
    putStrLn "=== Tests de elemSeq ==="
    
    -- Création de séquences de test
    let intSeq :: Sequence Int
        intSeq = fromList [1, 2, 3, 4, 5]
        
    let strSeq :: Sequence String
        strSeq = fromList ["apple", "banana", "cherry"]
        
    let emptySeq :: Sequence Int
        emptySeq = empty
    
    -- Tests avec des entiers
    putStrLn "Tests avec Sequence Int:"
    putStrLn $ "elemSeq 3 [1,2,3,4,5]: " ++ show (elemSeq 3 intSeq)  -- True
    putStrLn $ "elemSeq 7 [1,2,3,4,5]: " ++ show (elemSeq 7 intSeq)  -- False
    putStrLn $ "elemSeq 1 [1,2,3,4,5]: " ++ show (elemSeq 1 intSeq)  -- True
    putStrLn $ "elemSeq 5 [1,2,3,4,5]: " ++ show (elemSeq 5 intSeq)  -- True
    
    -- Tests avec des strings
    putStrLn "\nTests avec Sequence String:"
    putStrLn $ "elemSeq \"banana\" [\"apple\",\"banana\",\"cherry\"]: " ++ 
               show (elemSeq "banana" strSeq)  -- True
    putStrLn $ "elemSeq \"orange\" [\"apple\",\"banana\",\"cherry\"]: " ++ 
               show (elemSeq "orange" strSeq)  -- False
    
    -- Tests avec séquence vide
    putStrLn "\nTests avec séquence vide:"
    putStrLn $ "elemSeq 1 empty: " ++ show (elemSeq 1 emptySeq)  -- False
    
    -- Tests des versions alternatives
    putStrLn "\nTests des versions alternatives:"
    putStrLn $ "elemSeq' 3 [1,2,3,4,5]: " ++ show (elemSeq' 3 intSeq)  -- True
    putStrLn $ "elemSeqFold 4 [1,2,3,4,5]: " ++ show (elemSeqFold 4 intSeq)  -- True
    putStrLn $ "elemSeqFoldR 7 [1,2,3,4,5]: " ++ show (elemSeqFoldR 7 intSeq)  -- False
    
    -- Test de cas limite
    putStrLn "\nTest de cas limite:"
    let singleSeq = fromList [42]
    putStrLn $ "elemSeq 42 [42]: " ++ show (elemSeq 42 singleSeq)  -- True
    putStrLn $ "elemSeq 99 [42]: " ++ show (elemSeq 99 singleSeq)  -- False