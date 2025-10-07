-- Fonction prenant poids (kg) et taille (m), retournant la catégorie IMC
bmiCategory :: Float -> Float -> String
bmiCategory weight height
    | bmi < 18.5          = "Underweight"  -- Maigreur
    | bmi < 25.0          = "Normal"       -- Normal (18.5 ≤ bmi < 25)
    | bmi < 30.0          = "Overweight"   -- Surpoids
    | otherwise           = "Obese"        -- Obésité (bmi ≥ 30)
  where
    bmi = weight / (height ^ 2)  -- Calcul de l'IMC

-- Tests
main :: IO ()
main = do
    print (bmiCategory 70 1.75)  -- "Normal" (~22.86)
    print (bmiCategory 90 1.8)   -- "Overweight" (~27.78)
