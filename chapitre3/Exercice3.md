import Text.Printf (printf)  -- Pour formater en hexadécimal

-- La fonction prend un tuple (r, g, b) d'entiers et renvoie une chaîne
rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r, g, b) =
    -- let permet de définir des variables intermédiaires
    let
        -- %02X => format hex sur 2 caractères (complété par 0 si besoin, en majuscules)
        redHex   = printf "%02X" r
        greenHex = printf "%02X" g
        blueHex  = printf "%02X" b
    in
        -- On concatène les trois valeurs hex
        redHex ++ greenHex ++ blueHex

-- Tests
main :: IO ()
main = do
    print (rgbToHex (255, 0, 127)) -- "FF007F"
    print (rgbToHex (0, 255, 64))  -- "00FF40"
