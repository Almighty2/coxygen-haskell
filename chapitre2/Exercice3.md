myAge :: Int
myAge = 25

piValue :: Double
piValue = 3.14159

greeting :: String
greeting = "Hello, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True

main :: IO ()
main = do
  putStrLn $ "My age: " ++ show myAge
  putStrLn $ "Pi value: " ++ show piValue
  putStrLn $ "Greeting: " ++ greeting
  putStrLn $ "Is Haskell fun? " ++ show isHaskellFun
