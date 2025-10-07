-- Définition du type Book avec record syntax
-- On ajoute "deriving Show" pour pouvoir l’afficher directement
data Book = Book
    { title  :: String
    , author :: String
    , year   :: Int
    } deriving (Show)

-- Exemple de livre
book1 :: Book
book1 = Book
    { title = "Learn You a Haskell for Great Good!"
    , author = "Miran Lipovača"
    , year = 2011
    }

-- Exemple d’utilisation
main :: IO ()
main = do
    -- Grâce à deriving (Show), on peut directement afficher la valeur
    print book1
