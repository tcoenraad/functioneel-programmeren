import FPPrac
import Data.Char
import Data.List

type Person = (String, Number, Char, String)
type Db = [Person] 
database :: Db
database = [("Truus", 35, 'v', "Lutjebroek"), ("Sophie", 20, 'v', "Spijkerbroek"), ("Thijs-jan", 35, 'm', "Broekje")]

doorzoekDatabase :: Db -> String -> (String, Number, Char, String)
doorzoekDatabase db query_name | map toLower name == map toLower query_name = head(db)
                               | otherwise = doorzoekDatabase (tail db) query_name
                                  where (name, _, _, _) = head(db)
doorzoekDatabaseEnVindLeeftijd :: Db -> String -> Number
doorzoekDatabaseEnVindLeeftijd db query_name = leeftijd (doorzoekDatabase db query_name)

naam :: Person -> String
naam (naam, _, _, _) = naam

leeftijd :: Person -> Number
leeftijd (_, leeftijd, _, _) = leeftijd

geslacht :: Person -> Char
geslacht (_, _, geslacht, _) = geslacht

woonplaats :: Person -> String
woonplaats (_, _, _, woonplaats) = woonplaats

leeftijdPlusNRecursief :: Number -> Db -> Db
leeftijdPlusNRecursief n [] = []
leeftijdPlusNRecursief n (x:xs) = [(naam x, leeftijd x + n, geslacht x, woonplaats x)] ++ leeftijdPlusNRecursief n xs

leeftijdPlusNLijstcomprehensie :: Number -> Db -> Db
leeftijdPlusNLijstcomprehensie n db = [(naam x, leeftijd x + n, geslacht x, woonplaats x) | x <- db]

leeftijdPlusNHogereOrde :: Number -> Db -> Db
leeftijdPlusNHogereOrde n db = map (\x -> (naam x, leeftijd x + n, geslacht x, woonplaats x)) db

vrouwenFilterRecursief :: Db -> [String]
vrouwenFilterRecursief [] = []
vrouwenFilterRecursief (x:xs) | geslacht x == 'v' && leeftijd x >= 30 && leeftijd x <= 40 = [naam x] ++ vrouwenFilterRecursief xs
                              | otherwise = vrouwenFilterRecursief xs

vrouwenFilterLijstcomprehensie :: Db -> [String]
vrouwenFilterLijstcomprehensie db = [naam x | x <- db, geslacht x == 'v' && leeftijd x >= 30 && leeftijd x <= 40]

vrouwenFilterHogereOrde :: Db -> [String]
vrouwenFilterHogereOrde db = map (\x -> naam x) (filter (\x -> geslacht x == 'v' && leeftijd x >= 30 && leeftijd x <= 40) db)

sorteerOpLeeftijd :: Db -> Db
sorteerOpLeeftijd db = sortBy sorteerPersoon db

sorteerPersoon :: Person -> Person -> Ordering
sorteerPersoon p1 p2 = compare (leeftijd p1) (leeftijd p2)
