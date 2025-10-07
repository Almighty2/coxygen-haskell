-- HC2T7 - Boolean Expressions

trueWithAnd :: Bool
trueWithAnd = True && True

falseWithOr :: Bool
falseWithOr = False || False

trueWithNot :: Bool
trueWithNot = not False

falseComparison :: Bool
falseComparison = 3 > 5

main :: IO ()
main = do
  putStrLn $ "True && True         = " ++ show trueWithAnd
  putStrLn $ "False || False       = " ++ show falseWithOr
  putStrLn $ "not False            = " ++ show trueWithNot
  putStrLn $ "3 > 5 (comparison)   = " ++ show falseComparison
