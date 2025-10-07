-- Fonction qui prend une note et retourne un commentaire adapté
gradeComment :: Int -> String
gradeComment grade
    | grade >= 90 && grade <= 100 = "Excellent!"
    | grade >= 70 && grade <= 89  = "Good job!"
    | grade >= 50 && grade <= 69  = "You passed."
    | grade >= 0  && grade <= 49  = "Better luck next time."
    | otherwise                   = "Invalid grade"

-- Tests
main :: IO ()
main = do
    print (gradeComment 95)  -- "Excellent!"
    print (gradeComment 75)  -- "Good job!"
    print (gradeComment 55)  -- "You passed."
    print (gradeComment 30)  -- "Better luck next time."
    print (gradeComment 120) -- "Invalid grade"
