-- On importe les fonctions nécessaires :
-- `sortBy` permet de trier une liste avec un critère personnalisé
-- `comparing` facilite la comparaison basée sur une clé spécifique (ici, les scores)
import Data.List (sortBy)
import Data.Ord (comparing)

-- Fonction qui extrait uniquement les noms des joueurs d’une liste de tuples (nom, score)
extractPlayers :: [(String, Int)] -> [String]
extractPlayers xs = [name | (name, _) <- xs]
-- On utilise une compréhension de liste pour ignorer le score (_) et garder uniquement le nom

-- Fonction qui trie les joueurs par score décroissant
sortByScore :: [(String, Int)] -> [(String, Int)]
sortByScore = sortBy (flip (comparing snd))
-- `snd` permet d'accéder au score dans le tuple
-- `comparing snd` crée une fonction de tri par score croissant
-- `flip` inverse l'ordre du tri pour le rendre décroissant (du plus grand au plus petit)

-- Fonction qui garde uniquement les 3 premiers éléments d'une liste (top 3)
topThree :: [(String, Int)] -> [(String, Int)]
topThree xs = take 3 xs

-- Fonction principale qui renvoie les noms des 3 meilleurs joueurs
getTopThreePlayers :: [(String, Int)] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore
-- On compose les fonctions : d'abord trier, puis prendre les 3 premiers, puis extraire les noms

-- Fonction main qui teste tout cela
main :: IO ()
main = do
    -- Définition de la liste des joueurs avec leurs scores
    let players = [("Alice", 50), ("Bob", 70), ("Charlie", 90), ("David", 30)]
    
    -- Affichage du résultat : les 3 meilleurs joueurs (noms uniquement)
    print (getTopThreePlayers players)
