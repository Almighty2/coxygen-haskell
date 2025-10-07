{-
HC9T1: Définir un Synonyme de Type Paramétrique
Créez un synonyme de type paramétrique appelé Entity a pour représenter
divers types d'entités avec des adresses.

Explication :
- Un synonyme de type crée un alias pour un type existant
- Paramétrique signifie qu'il prend un paramètre de type 'a'
- Nous voulons représenter des entités qui ont des adresses
- Cas d'usage courant : entités comme Personne, Entreprise, etc. avec différents types d'adresse
-}

-- | Synonyme de type paramétrique pour les entités avec adresses
-- Entity a représente tout type d'entité qui possède un champ adresse
-- Le paramètre de type 'a' permet une flexibilité sur le type de l'adresse
type Entity a = (String, a)  -- (NomEntité, Adresse)

-- Représentation alternative avec syntaxe record (commentée car ce n'est pas un simple synonyme)
-- data Entity a = Entity { nom :: String, adresse :: a }

-- Exemples d'utilisation avec différents types d'adresse :

-- Adresse comme une simple chaîne de caractères
type EntiteSimple = Entity String

-- Adresse comme un tuple (Rue, Ville, CodePostal)
type EntiteDetaillee = Entity (String, String, String)

-- Adresse comme un type record personnalisé
data Adresse = Adresse {
    rue :: String,
    ville :: String,
    codePostal :: String,
    pays :: String
} deriving (Show)

type EntitePersonnalisee = Entity Adresse

-- Exemples d'instances :
personneExemple :: EntiteSimple
personneExemple = ("Jean Dupont", "123 Rue Principale, Paris")

entrepriseExemple :: EntiteDetaillee
entrepriseExemple = ("Société XYZ", ("456 Avenue des Champs", "Lyon", "69000"))

adresseComplete :: Adresse
adresseComplete = Adresse {
    rue = "789 Boulevard Saint-Michel",
    ville = "Marseille",
    codePostal = "13000",
    pays = "France"
}

clientExemple :: EntitePersonnalisee
clientExemple = ("Client Premium", adresseComplete)

-- Fonction utilitaire pour obtenir le nom d'une entité
getNom :: Entity a -> String
getNom (nom, _) = nom

-- Fonction utilitaire pour obtenir l'adresse d'une entité
getAdresse :: Entity a -> a
getAdresse (_, adresse) = adresse

-- Exemple d'utilisation des fonctions
main :: IO ()
main = do
    putStrLn $ "Nom de la personne: " ++ getNom personneExemple
    putStrLn $ "Adresse de l'entreprise: " ++ show (getAdresse entrepriseExemple)
    putStrLn $ "Ville du client: " ++ ville (getAdresse clientExemple)