{-
HC9T6: Définir un Type de Données Récursif
Définissez un type de données récursif Tweet qui représente un tweet avec
contenu, likes et commentaires, qui sont eux-mêmes des tweets.

Explication :
- Type de données récursif avec un champ qui contient le même type
- Structure arborescente pour les commentaires
- Gestion de la récursion terminale pour éviter les débordements de pile
-}

import Data.Char (toUpper)
import Data.List (maximumBy)
import Data.Ord (comparing)

-- | Type de données récursif Tweet représentant un tweet avec ses commentaires
data Tweet = Tweet
    { content   :: String     -- ^ Contenu du tweet
    , likes     :: Int        -- ^ Nombre de likes
    , comments  :: [Tweet]    -- ^ Liste de tweets commentaires (récursif)
    }
    deriving (Show, Eq)  -- Dérivation pour l'affichage et la comparaison

-- | Tweet vide ou nul pour représenter l'absence de tweet
tweetVide :: Tweet
tweetVide = Tweet "" 0 []

-- | Crée un tweet simple sans commentaires
tweetSimple :: String -> Int -> Tweet
tweetSimple contenu nbLikes = Tweet contenu nbLikes []

-- | Ajoute un commentaire à un tweet
ajouterCommentaire :: Tweet -> Tweet -> Tweet
ajouterCommentaire tweet commentaire = 
    tweet { comments = commentaire : comments tweet }

-- | Ajoute plusieurs commentaires à un tweet
ajouterCommentaires :: Tweet -> [Tweet] -> Tweet
ajouterCommentaires tweet nouveauxCommentaires =
    tweet { comments = nouveauxCommentaires ++ comments tweet }

-- | Incrémente le nombre de likes d'un tweet
incrementerLikes :: Tweet -> Tweet
incrementerLikes tweet = tweet { likes = likes tweet + 1 }

-- | Modifie le contenu d'un tweet
modifierContenu :: String -> Tweet -> Tweet
modifierContenu nouveauContenu tweet = tweet { content = nouveauContenu }

-- | Calcule le nombre total de likes (tweet + tous ses commentaires)
totalLikes :: Tweet -> Int
totalLikes tweet = likes tweet + sum (map totalLikes (comments tweet))

-- | Calcule le nombre total de tweets dans l'arborescence (inclut le tweet racine)
totalTweets :: Tweet -> Int
totalTweets tweet = 1 + sum (map totalTweets (comments tweet))

-- | Calcule la profondeur maximale de l'arborescence des commentaires
profondeurMax :: Tweet -> Int
profondeurMax tweet
    | null (comments tweet) = 1
    | otherwise = 1 + maximum (map profondeurMax (comments tweet))

-- | Affiche un tweet avec son arborescence de commentaires (indentation)
afficherTweet :: Tweet -> String
afficherTweet tweet = afficherTweetNiveau tweet 0
  where
    afficherTweetNiveau :: Tweet -> Int -> String
    afficherTweetNiveau t niveau =
        let indent = replicate (niveau * 2) ' '
            lignePrincipale = indent ++ "📝 " ++ content t ++ " (" ++ show (likes t) ++ " likes)\n"
            commentairesStr = concatMap (\c -> afficherTweetNiveau c (niveau + 1)) (comments t)
        in lignePrincipale ++ commentairesStr

-- | Recherche des tweets contenant un mot-clé
rechercherMotCle :: String -> Tweet -> [Tweet]
rechercherMotCle motCle tweet
    | motCle `elem` words (content tweet) = tweet : concatMap (rechercherMotCle motCle) (comments tweet)
    | otherwise = concatMap (rechercherMotCle motCle) (comments tweet)

-- | Trouve le tweet avec le plus de likes
tweetPlusPopulaire :: Tweet -> Tweet
tweetPlusPopulaire tweet =
    let commentairesPopulaires = map tweetPlusPopulaire (comments tweet)
        tousLesTweets = tweet : commentairesPopulaires
    in maximumBy (comparing likes) tousLesTweets

-- | Fonction pour transformer le contenu d'un tweet
transformerContenu :: (String -> String) -> Tweet -> Tweet
transformerContenu f tweet = tweet { content = f (content tweet) }

-- | Exemples de tweets
tweetPrincipal :: Tweet
tweetPrincipal = Tweet
    { content = "Bonjour tout le monde ! 🌟"
    , likes = 150
    , comments =
        [ Tweet
            { content = "Super tweet ! 👍"
            , likes = 25
            , comments =
                [ Tweet "Merci ! 😊" 5 []
                , Tweet "Je suis d'accord !" 3 []
                ]
            }
        , Tweet
            { content = "Intéressant, peux-tu développer ?"
            , likes = 12
            , comments = []
            }
        , Tweet
            { content = "J'adore ! ❤️"
            , likes = 18
            , comments =
                [ Tweet "Moi aussi !" 2 []
                ]
            }
        ]
    }

-- | Crée une structure de tweets plus complexe
creerStructureComplexe :: Tweet
creerStructureComplexe =
    let tweet1 = tweetSimple "Premier tweet de la journée" 100
    in foldl ajouterCommentaire tweet1
        [ tweetSimple "Commentaire niveau 1" 20
        , tweetSimple "Autre commentaire" 15
        ]

-- | Démonstration principale
main :: IO ()
main = do
    putStrLn "=== Type de données récursif Tweet ==="
    
    -- Affichage du tweet principal
    putStrLn $ afficherTweet tweetPrincipal
    
    -- Statistiques
    putStrLn "=== Statistiques ==="
    putStrLn $ "Total de likes: " ++ show (totalLikes tweetPrincipal)
    putStrLn $ "Total de tweets: " ++ show (totalTweets tweetPrincipal)
    putStrLn $ "Profondeur max: " ++ show (profondeurMax tweetPrincipal)
    
    -- Recherche
    putStrLn $ "\n=== Recherche de 'Super' ==="
    let tweetsTrouves = rechercherMotCle "Super" tweetPrincipal
    mapM_ (putStrLn . content) tweetsTrouves
    
    -- Tweet le plus populaire
    putStrLn $ "\n=== Tweet le plus populaire ==="
    let populaire = tweetPlusPopulaire tweetPrincipal
    putStrLn $ "Contenu: " ++ content populaire
    putStrLn $ "Likes: " ++ show (likes populaire)
    
    -- Modification
    putStrLn $ "\n=== Modification ==="
    let tweetModifie = incrementerLikes tweetPrincipal
    putStrLn $ "Nouveau nombre de likes: " ++ show (likes tweetModifie)
    
    let tweetRenomme = modifierContenu "Nouveau contenu !" tweetPrincipal
    putStrLn $ "Nouveau contenu: " ++ content tweetRenomme
    
    -- Transformation du contenu
    putStrLn $ "\n=== Transformation du contenu ==="
    let tweetMajuscules = transformerContenu (map toUpper) tweetPrincipal
    putStrLn $ "Contenu en majuscules: " ++ content tweetMajuscules

-- | Fonctions avancées pour manipuler l'arborescence

-- | Applique une fonction à tous les tweets de l'arborescence
mapTweet :: (Tweet -> Tweet) -> Tweet -> Tweet
mapTweet f tweet = f tweet { comments = map (mapTweet f) (comments tweet) }

-- | Parcours en largeur de l'arborescence
parcoursLargeur :: Tweet -> [Tweet]
parcoursLargeur tweet = bfs [tweet]
  where
    bfs [] = []
    bfs (x:xs) = x : bfs (xs ++ comments x)

-- | Parcours en profondeur de l'arborescence
parcoursProfondeur :: Tweet -> [Tweet]
parcoursProfondeur tweet = tweet : concatMap parcoursProfondeur (comments tweet)

-- | Filtrer les tweets selon un prédicat
filtrerTweets :: (Tweet -> Bool) -> Tweet -> [Tweet]
filtrerTweets predicat tweet
    | predicat tweet = tweet : concat
    | otherwise = concat
  where
    concat = concatMap (filtrerTweets predicat) (comments tweet)

-- | Exemple de filtrage
tweetsAvecBeaucoupDeLikes :: Tweet -> [Tweet]
tweetsAvecBeaucoupDeLikes = filtrerTweets (\t -> likes t > 10)

-- | Fonction pour mettre en majuscule tout le contenu d'une arborescence
mettreEnMajuscules :: Tweet -> Tweet
mettreEnMajuscules = mapTweet (transformerContenu (map toUpper))

-- | Démonstration de la transformation complète
demoTransformationComplete :: IO ()
demoTransformationComplete = do
    putStrLn $ "\n=== Transformation complète en majuscules ==="
    let tweetTransforme = mettreEnMajuscules tweetPrincipal
    putStrLn $ afficherTweet tweetTransforme