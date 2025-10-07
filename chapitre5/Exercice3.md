import Data.Char (isUpper)

-- Fonction qui vérifie si un mot commence par une majuscule
startsWithUpper :: String -> Bool
startsWithUpper []    = False  -- chaîne vide, pas de majuscule
startsWithUpper (x:_) = isUpper x

-- Fonction qui vérifie si une liste de mots contient un mot commençant par majuscule
containsUppercaseWord :: [String] -> Bool
containsUppercaseWord wordsList = any startsWithUpper wordsList

-- Tests
main :: IO ()
main = do
    print (containsUppercaseWord ["hello", "World", "haskell"])  -- True
    print (containsUppercaseWord ["hello", "world", "haskell"])  -- False
