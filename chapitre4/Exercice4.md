specialBirthday :: Int -> String
specialBirthday 18 = "Happy 18th Birthday!"
specialBirthday 21 = "Welcome to adulthood!"
specialBirthday 30 = "The big 3-0!"
specialBirthday _  = "Just another day."

main :: IO ()
main = do
    print (specialBirthday 18)  -- Affiche "Happy 18th Birthday!"
    print (specialBirthday 25)  -- Affiche "Just another day."
