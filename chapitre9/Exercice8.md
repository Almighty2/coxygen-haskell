{-# LANGUAGE FlexibleInstances #-}

module Main where

-- Définition du type récursif Sequence a
-- Représente une séquence linéaire de nœuds
data Sequence a = Empty | Node a (Sequence a) deriving (Show, Eq)

-- | Crée une séquence vide
empty :: Sequence a
empty = Empty

-- | Crée un nœud avec une valeur et une séquence suivante
node :: a -> Sequence a -> Sequence a
node = Node

-- | Convertit une liste en Sequence
fromList :: [a] -> Sequence a
fromList [] = Empty
fromList (x:xs) = Node x (fromList xs)

-- | Convertit une Sequence en liste
toList :: Sequence a -> [a]
toList Empty = []
toList (Node x next) = x : toList next

-- | Ajoute un élément au début de la séquence
cons :: a -> Sequence a -> Sequence a
cons x seq = Node x seq

-- | Récupère le premier élément de la séquence (s'il existe)
head' :: Sequence a -> Maybe a
head' Empty = Nothing
head' (Node x _) = Just x

-- | Récupère la queue de la séquence (sans le premier élément)
tail' :: Sequence a -> Maybe (Sequence a)
tail' Empty = Nothing
tail' (Node _ next) = Just next

-- | Longueur de la séquence
length' :: Sequence a -> Int
length' Empty = 0
length' (Node _ next) = 1 + length' next

-- | Vérifie si la séquence est vide
isEmpty :: Sequence a -> Bool
isEmpty Empty = True
isEmpty _ = False

-- | Concatène deux séquences
concat' :: Sequence a -> Sequence a -> Sequence a
concat' Empty seq2 = seq2
concat' (Node x next) seq2 = Node x (concat' next seq2)

-- | Inverse une séquence
reverse' :: Sequence a -> Sequence a
reverse' seq = reverseHelper seq Empty
  where
    reverseHelper Empty acc = acc
    reverseHelper (Node x next) acc = reverseHelper next (Node x acc)

-- | Applique une fonction à tous les éléments d'une séquence
map' :: (a -> b) -> Sequence a -> Sequence b
map' _ Empty = Empty
map' f (Node x next) = Node (f x) (map' f next)

-- | Filtre les éléments d'une séquence selon un prédicat
filter' :: (a -> Bool) -> Sequence a -> Sequence a
filter' _ Empty = Empty
filter' p (Node x next)
    | p x       = Node x (filter' p next)
    | otherwise = filter' p next

-- | Fold left sur une séquence
foldl' :: (b -> a -> b) -> b -> Sequence a -> b
foldl' _ acc Empty = acc
foldl' f acc (Node x next) = foldl' f (f acc x) next

-- | Fold right sur une séquence
foldr' :: (a -> b -> b) -> b -> Sequence a -> b
foldr' _ acc Empty = acc
foldr' f acc (Node x next) = f x (foldr' f acc next)

-- | Prend les n premiers éléments d'une séquence
take' :: Int -> Sequence a -> Sequence a
take' 0 _ = Empty
take' _ Empty = Empty
take' n (Node x next) = Node x (take' (n - 1) next)

-- | Supprime les n premiers éléments d'une séquence
drop' :: Int -> Sequence a -> Sequence a
drop' 0 seq = seq
drop' _ Empty = Empty
drop' n (Node _ next) = drop' (n - 1) next

-- Instance de Functor pour pouvoir utiliser fmap
instance Functor Sequence where
    fmap = map'

-- Instance de Foldable pour pouvoir utiliser les fonctions de Data.Foldable
instance Foldable Sequence where
    foldr = foldr'
    foldl = foldl'

-- Fonction main pour exécuter les tests
main :: IO ()
main = do
    putStrLn "=== Tests de la Sequence a ==="
    
    -- Création de séquences avec des types explicites
    let seq1 :: Sequence Int
        seq1 = fromList [1, 2, 3, 4, 5]
        
    let seq2 :: Sequence String
        seq2 = fromList ["a", "b", "c"]
        
    let emptySeq :: Sequence Int  -- Spécification du type
        emptySeq = empty
    
    putStrLn $ "seq1: " ++ show seq1
    putStrLn $ "seq2: " ++ show seq2
    putStrLn $ "emptySeq: " ++ show emptySeq
    
    -- Test des fonctions de base
    putStrLn $ "head' seq1: " ++ show (head' seq1)
    putStrLn $ "tail' seq1: " ++ show (tail' seq1)
    putStrLn $ "length' seq1: " ++ show (length' seq1)
    putStrLn $ "isEmpty emptySeq: " ++ show (isEmpty emptySeq)
    putStrLn $ "isEmpty seq1: " ++ show (isEmpty seq1)
    
    -- Test de concaténation
    let concatenated = concat' seq1 (fromList [6, 7, 8])
    putStrLn $ "concat' seq1 [6,7,8]: " ++ show concatenated
    putStrLn $ "toList: " ++ show (toList concatenated)
    
    -- Test de map et filter
    let mapped = map' (* 2) seq1
    putStrLn $ "map' (*2) seq1: " ++ show mapped
    
    let filtered = filter' (> 3) seq1
    putStrLn $ "filter' (>3) seq1: " ++ show filtered
    
    -- Test de fold
    putStrLn $ "foldl' (+) 0 seq1: " ++ show (foldl' (+) 0 seq1)
    putStrLn $ "foldr' (:) [] seq1: " ++ show (foldr' (:) [] seq1)
    
    -- Test de take et drop
    putStrLn $ "take' 3 seq1: " ++ show (take' 3 seq1)
    putStrLn $ "drop' 2 seq1: " ++ show (drop' 2 seq1)
    
    -- Test de reverse
    putStrLn $ "reverse' seq1: " ++ show (reverse' seq1)
    
    -- Test avec Functor (fmap)
    let fmapped = fmap (+ 10) seq1
    putStrLn $ "fmap (+10) seq1: " ++ show fmapped