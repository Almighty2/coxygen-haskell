smallNumber :: Int
smallNumber = 2^62

bigNumber :: Integer
bigNumber = 2^127

main :: IO ()
main = do
  putStrLn $ "smallNumber (2^62) as Int     = " ++ show smallNumber
  putStrLn $ "bigNumber (2^127) as Integer  = " ++ show bigNumber
  putStrLn $ "2^64 as Int (overflow test)   = " ++ show (2^64 :: Int)
