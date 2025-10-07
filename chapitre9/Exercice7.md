module Main where

-- Définition du type pour un Tweet
-- Un Tweet a des likes et une liste de commentaires (qui sont aussi des Tweets)
data Tweet = Tweet {
    likes :: Int,
    comments :: [Tweet]
} deriving (Show)

-- Fonction pour calculer l'engagement d'un tweet
-- Engagement = likes du tweet + somme des engagements de tous les commentaires
engagement :: Tweet -> Int
engagement tweet = 
    let tweetLikes = likes tweet  -- Nombre de likes du tweet principal
        -- Calcul récursif de l'engagement de tous les commentaires
        commentsEngagement = sum (map engagement (comments tweet))
    in tweetLikes + commentsEngagement

-- Fonction alternative avec pattern matching pour plus de clarté
engagement' :: Tweet -> Int
engagement' (Tweet l cs) = l + sum (map engagement' cs)

-- Version avec gestion des cas vides (plus robuste)
engagementSafe :: Tweet -> Int
engagementSafe tweet = likes tweet + sum (map engagementSafe (comments tweet))

-- Exemples de tweets pour tester
-- Tweet simple sans commentaires
simpleTweet :: Tweet
simpleTweet = Tweet {likes = 15, comments = []}

-- Tweet avec commentaires imbriqués
complexTweet :: Tweet
complexTweet = Tweet {
    likes = 10,
    comments = [
        Tweet {likes = 5, comments = []},
        Tweet {
            likes = 3, 
            comments = [
                Tweet {likes = 2, comments = []},
                Tweet {likes = 1, comments = []}
            ]
        },
        Tweet {likes = 4, comments = []}
    ]
}

-- Fonction main pour exécuter les tests
main :: IO ()
main = do
    putStrLn "Tests de la fonction engagement:"
    
    -- Test 1: Tweet simple
    putStrLn $ "Tweet simple: " ++ show (engagement simpleTweet)
    -- Résultat attendu: 15
    
    -- Test 2: Tweet complexe
    putStrLn $ "Tweet complexe: " ++ show (engagement complexTweet)
    -- Calcul: 10 + (5 + 0) + (3 + (2 + 0 + 1 + 0)) + (4 + 0) = 10 + 5 + 6 + 4 = 25
    
    -- Test avec la version alternative
    putStrLn $ "Version alternative: " ++ show (engagement' complexTweet)
    
    -- Test avec la version safe
    putStrLn $ "Version safe: " ++ show (engagementSafe complexTweet)