-- Définition d'un type User
data User = User
    { username :: String
    , email    :: String
    , age      :: Int
    } deriving (Show)

-- Définition de la type class ShowDetailed
class ShowDetailed a where
    showDetailed :: a -> String

-- Implémentation de ShowDetailed pour User
instance ShowDetailed User where
    showDetailed user =
        "Username: " ++ username user ++
        ", Email: " ++ email user ++
        ", Age: " ++ show (age user)

-- Exemple d'utilisation
main :: IO ()
main = do
    let user1 = User { username = "alice", email = "alice@example.com", age = 30 }
    putStrLn (showDetailed user1)
