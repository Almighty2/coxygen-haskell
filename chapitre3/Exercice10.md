-- Fonction qui vérifie si une chaîne est un palindrome
isPalindrome :: String -> Bool
isPalindrome str
    | length str <= 1 = True  -- Si la chaîne est vide ou un seul caractère, c'est un palindrome
    | head str == last str = isPalindrome (init (tail str))  -- Si premier et dernier caractère égaux, on vérifie la sous-chaîne centrale
    | otherwise = False  -- Sinon, ce n'est pas un palindrome

-- Tests
main :: IO ()
main = do
    print (isPalindrome "racecar")  -- True
    print (isPalindrome "haskell")  -- False
    print (isPalindrome "madam")    -- True
