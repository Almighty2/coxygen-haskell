-- On reprend nos synonymes de types pour plus de clarté
type Address = String
type Value   = Int

-- Définition d’un type Transaction avec record syntax
data Transaction = Transaction
    { from          :: Address
    , to            :: Address
    , amount        :: Value
    , transactionId :: String
    } deriving (Show)

-- Fonction qui crée une transaction
-- Pour simplifier, l’ID sera construit à partir des adresses et du montant
createTransaction :: Address -> Address -> Value -> String
createTransaction fromAddr toAddr val =
    let tx = Transaction
                { from = fromAddr
                , to = toAddr
                , amount = val
                , transactionId = fromAddr ++ "-" ++ toAddr ++ "-" ++ show val
                }
    in transactionId tx   -- On renvoie uniquement l’ID de la transaction

-- Exemple d’utilisation
main :: IO ()
main = do
    let txId = createTransaction "0xABC123" "0xDEF456" 100
    putStrLn ("Transaction ID généré : " ++ txId)
