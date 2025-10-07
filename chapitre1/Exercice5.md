-- Fonction qui génère une liste infinie de nombres à partir de 1
infiniteNumbers :: [Int]
infiniteNumbers = [1..]
-- Grâce à la paresse (laziness) de Haskell, cette liste n’est évaluée que partiellement

-- Fonction qui extrait les n premiers éléments de cette liste infinie
firstN :: Int -> [Int]
firstN n = take n infiniteNumbers
-- `take n` extrait les n premiers éléments sans évaluer toute la liste

-- Fonction principale pour tester avec n = 10
main :: IO ()
main = do
    let n = 10
    putStrLn ("Les " ++ show n ++ " premiers nombres sont :")
    print (firstN n)