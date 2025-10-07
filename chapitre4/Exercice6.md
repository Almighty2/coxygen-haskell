-- Fonction qui analyse la liste et retourne un message selon son contenu
whatsInsideThisList :: [a] -> String
whatsInsideThisList []       = "The list is empty."
whatsInsideThisList [x]      = "The list has one element."
whatsInsideThisList [x, y]   = "The list has two elements."
whatsInsideThisList (_:_:_:_) = "The list has more than two elements."

-- Tests
main :: IO ()
main = do
    print (whatsInsideThisList ([] :: [Int]))         -- "The list is empty."
    print (whatsInsideThisList [42])                   -- "The list has one element."
    print (whatsInsideThisList [1, 2])                 -- "The list has two elements."
    print (whatsInsideThisList [1, 2, 3])              -- "The list has more than two elements."
