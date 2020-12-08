import AdventData ( day2data, day3data, day4data, day5data, day6data, day7data, day8data)
import Text.Read ( readMaybe )
import Data.Maybe ()
import Data.List ( intercalate, nub, intersect )
import Data.Map (Map, fromList, keys, lookup)
import qualified Data.Map as Map

-- December 1st
-- From a list of integers, get the product of the two that sum to 2020
day1p1 :: [Int] -> Int
day1p1 [] = error "no viable sums"
day1p1 (x:xs) =
  case filter(\y -> x + y == 2020) xs of
    [] -> day1p1 xs
    [y] -> x * y

day1p2 :: [Int] -> Int
day1p2 [] = error "no viable sums"
day1p2 (x:xs) = 
  case dualsums xs of
    Just(prod) -> prod * x
    Nothing    -> day1p2 xs 
  where
    dualsums :: [Int] -> Maybe Int
    dualsums [] = Nothing
    dualsums (y:ys) =
      case filter (\z -> x + y + z == 2020) ys of
        [] -> dualsums ys
        [z] -> Just $ y * z

day1Data :: [Int]
day1Data = [1863, 1750, 1767, 1986, 1180, 1719, 1946, 1866, 1939, 1771, 1766, 1941, 1728, 1322, 1316, 1775, 1776, 1742, 1726, 1994, 1949, 1318, 1223, 1741, 1816, 1111, 1991, 1406, 1230, 1170, 1823, 1792, 1148, 1953, 1706, 1724, 1307, 1844, 1943, 1862, 1812, 1286, 1837, 1785, 1998, 1938, 1248, 1822, 1829, 1903, 1131, 1826, 1892, 1143, 1898, 1981, 1225, 1980, 1850, 1885, 324, 289, 1914, 1249, 1848, 1995, 1962, 1875, 1827, 1931, 1244, 1739, 1897, 1687, 1907, 1867, 1922, 1972, 1842, 1757, 1610, 1945, 1835, 1894, 1265, 1872, 1963, 1712, 891, 1813, 1800, 1235, 1879, 1732, 1522, 1335, 1936, 1830, 1772, 1700, 2005, 1253, 1836, 1935, 1137, 1951, 1849, 1883, 1192, 1824, 1918, 1965, 1759, 1195, 1882, 1748, 1168, 1200, 1761, 1896, 527, 1769, 1560, 1947, 1997, 1461, 1828, 1801, 1877, 1900, 1924, 1782, 1718, 515, 1814, 1744, 1126, 1791, 1149, 1932, 1690, 1707, 1808, 1957, 1313, 1132, 1942, 1934, 1798, 2009, 1708, 1774, 1710, 1797, 1747, 959, 1955, 1717, 1716, 1290, 1654, 1857, 1968, 1874, 1853, 1175, 1493, 1425, 1125, 1973, 1790, 467, 1804, 987, 1944, 2001, 1895, 1917, 1218, 1147, 1884, 1819, 1179, 1859, 620, 1219, 2008, 1871, 1852, 1263, 1751, 1989, 1381, 1250, 1754, 1725, 1665, 1352, 1805, 1325] 

runDay1 :: IO()
runDay1 = do
  print $ day1p1 day1Data
  print $ day1p2 day1Data

-- December 2nd
-- From a list of policies, and passwords, determine how many allowed passwords exist.
type Day2Data = (Int, Int, Char, String)
type Day2Check = Day2Data -> Bool

day2p1check :: Day2Check
day2p1check (min, max, c, cs) = count <= max && count >= min
  where
    count = length $ filter (==c) cs

day2p2check :: Day2Check
day2p2check (start, end, c, cs) = first /= last && (first == c || last == c)
  where
    first = cs!!(start-1)
    last = cs!!(end-1)

day2 :: Day2Check -> [Day2Data] -> Int
day2 = (length . ) . filter

runDay2 :: IO()
runDay2 = do
  print $ day2 day2p1check day2data
  print $ day2 day2p2check day2data

-- December 3rd
-- From a map of open spots, trees, and a slop to go down, calculate the number of trees you would hit.
type HillMap = ([[Char]], Int, Int)

day3p1 :: HillMap -> (Int,Int) -> Int
day3p1 (slope, width, height) (dx,dy) = length $ filter (=='#') $ map (\a -> slope!!(a*dy)!!(a*dx `mod` width)) [0..(height `div` dy) - 1]

day3p2 :: HillMap -> [(Int,Int)] -> Int
day3p2 a grads = foldr (*) 1 $ map (day3p1 a) grads

runDay3 ::IO()
runDay3 = do
  print $ day3p1 (day3data,31,323)  (3,1)
  print $ day3p2 (day3data,31,323) [(1,1),(3,1),(5,1),(7,1),(1,2)]

-- December 4th
-- From list of passport data, check for valid passports
type Passport = [(String, String)]
type Day4Filter = Passport -> Bool

getfields :: Passport -> [String]
getfields [] = []
getfields ((f,_):xs) = f:getfields xs

day4p1filter :: Day4Filter
day4p1filter pass = all (flip(elem) $ getfields pass) ["byr","iyr","eyr","hgt","hcl","ecl", "pid"]

day4p2filter :: Day4Filter
day4p2filter pass = day4p1filter pass && helper pass
  where
    validbyr, validiyr, valideyr, validhgt, validhcl, validecl, validpid :: String -> Bool
    validbyr z = i <= 2002 && i >= 1920
      where i = read z :: Int
    validiyr z = i <= 2020 && i >= 2010
      where i = read z :: Int
    valideyr z 
      = case readMaybe z :: Maybe Int of
        Just x ->  x <= 2030 && x >= 2020
        Nothing -> False
    validhgt z 
      = case take 2 $ reverse z of
        "mc" -> case readMaybe $ take 3 z :: Maybe Int of
                  Just x -> x >= 150 && x <= 193
                  Nothing -> False
        "ni" -> case readMaybe $ take 2 z :: Maybe Int of
                  Just x -> x >= 59 && x <= 76
                  Nothing -> False
        _ -> False
    validhcl ('#':z) = all (\y -> y `elem` "0123456789abcdef") z && length z == 6
    validhcl _ = False
    validecl = flip(elem) ["amb", "blu", "brn","gry","grn","hzl","oth"]
    validpid z = length z == 9 && all (flip(elem) "0123456789") z

    helper :: Day4Filter
    helper [] = True
    helper (x:xs) = rslt && helper xs
      where 
        rslt = 
          case x of
            ("byr", z) -> validbyr z
            ("iyr", z) -> validiyr z
            ("eyr", z) -> valideyr z
            ("hgt", z) -> validhgt z
            ("hcl", z) -> validhcl z
            ("ecl", z) -> validecl z
            ("pid", z) -> validpid z
            ("cid", _) -> True
            _          -> False

day4 :: Day4Filter -> [Passport] -> Int
day4 = (length.). filter

runDay4 :: IO()
runDay4 = do
  print $ day4 day4p1filter day4data
  print $ day4 day4p2filter day4data

-- December 5th
-- Find the seat

-- Tree revision
data SeatTree = Missing | Taken | Subtree SeatTree SeatTree
  deriving (Show)

addSeat :: SeatTree -> String -> SeatTree
addSeat Taken _ = error "duplicate Seat"
addSeat Missing "" = Taken
addSeat Missing (x:xs)
  | x == 'F' || x == 'L' = Subtree (addSeat Missing xs) Missing
  | otherwise = Subtree Missing (addSeat Missing xs)
addSeat (Subtree left right) (x:xs)
  | x == 'F' || x == 'L' = Subtree (addSeat left xs) right
  | otherwise = Subtree left (addSeat right xs)

constructseats :: [String] -> SeatTree
constructseats xs = helper Missing xs
  where
    helper :: SeatTree -> [String] -> SeatTree
    helper seats [] = seats
    helper seats (x:xs) = helper (addSeat seats x) xs

-- Actual workings for day
seatIDs :: [String] -> [Int]
seatIDs d = map (\(row,col) -> helper 128 row * 8 + helper 8 col) $ map (splitAt 7) d
  where
    helper :: Int -> String -> Int
    helper _ [x]
      | x == 'B' || x == 'R' = 1
      | otherwise = 0
    helper n (x:xs)
      | x == 'B' || x == 'R' = n' + helper n' xs
      | otherwise = helper n' xs
      where
        n' = n `div` 2

day5p1 :: [String] -> Int
day5p1 = maximum . seatIDs

day5p2 :: [String] -> Int
day5p2 seats = head [id | id <- [0..1024], not (id `elem` seatnums) && ((id + 1) `elem` seatnums && (id -1) `elem` seatnums)]
  where 
    seatnums = seatIDs seats

runDay5 :: IO()
runDay5 = do
  print $ day5p1 day5data
  print $ day5p2 day5data

-- December 6th
-- Count the number of "yes" answers to questions for each group

day6p1 :: [[String]] -> Int
day6p1 = sum . map(length . nub . concat)

day6p2 :: [[String]] -> Int
day6p2 = sum . map (length . foldr intersect ['a'..'z'])

runDay6 :: IO()
runDay6 = do
  print $ day6p1 day6data
  print $ day6p2 day6data

-- December 7th
-- Get the number of potential bags
day7p1 :: Map String [(Int, String)] -> Int
day7p1 bagmap = length $ filter (==True)  $ map (checkkey []) $ keys bagmap
  where
    checkkey :: [String] -> String -> Bool
    checkkey vstd k
      | k `elem` vstd = False
      | "shiny gold" `elem` keys' = True
      | otherwise = any (==True) $ map (checkkey (k:vstd)) keys'
      where
        keys' = map (\(_, z) -> z) $ case Data.Map.lookup k bagmap of
          Nothing -> []
          Just xs -> xs

day7p2 :: Map String [(Int, String)] -> Int
day7p2 bagmap = getnum "shiny gold" - 1
  where
    getnum :: String -> Int
    getnum k = case Data.Map.lookup k bagmap of
        Nothing -> 0
        Just [] -> 1
        Just ks' -> 1 + (sum $ map (\(n, k') -> n * getnum k') ks')

runDay7 :: IO()
runDay7 = do
  print $ day7p1 day7data
  print $ day7p2 day7data

-- December 8th
-- Run the assembly

day8p1 :: [(String, Int)] -> Int
day8p1 islst = runcode 0 0 []
  where
    runcode :: Int -> Int -> [Int] -> Int
    runcode instr acc vstd
      | instr `elem` vstd = acc
      | opc == "nop" = runcode (instr+1) acc (instr:vstd)
      | opc == "acc" = runcode (instr+1) (acc + opd) (instr:vstd)
      | opc == "jmp" = runcode (instr + opd) acc (instr:vstd)
      where
        (opc, opd) = islst!!instr

day8p2 :: [(String, Int)] -> Int
day8p2 islst = runcode 0 0 [] True
  where
    runcode :: Int -> Int -> [Int] -> Bool -> Int
    runcode is acc vstd s
      | is `elem` vstd = 0
      | is > 644 = 0
      | is < 0 = 0
      | is == 644 = acc
      | opc == "jmp" = jmp
      | opc == "nop" = nop
      | opc == "acc" = runcode (is+1) (acc + opd) (is:vstd) s
      where
        (opc, opd) = islst!!is
        jmp = runcode (is + opd) acc (is:vstd) s + if s then runcode (is+1) acc (is:vstd) False else 0
        nop = runcode (is+1) acc (is:vstd) s + if s then runcode (is + opd) acc (is:vstd) False else 0

runDay8 :: IO()
runDay8 = do
  print $ day8p1 day8data
  print $ day8p2 day8data

main :: IO ()
main = do
  runDay8