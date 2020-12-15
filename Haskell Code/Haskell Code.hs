
import AdventData
import Text.Read ( readMaybe )
import Data.Maybe (fromJust)
import Data.List (foldl', intersperse, group, groupBy, sort,  intercalate, nub, intersect )
import qualified Data.Map (adjust, Map, fromList, keys, lookup, filter)
import qualified Data.Map as Map
import Data.Bits

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



runDay1 :: IO()
runDay1 = do
  print $ day1p1 day1data
  print $ day1p2 day1data

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
day7p1 :: Map.Map String [(Int, String)] -> Int
day7p1 bagmap = length $ filter (==True)  $ map (checkkey []) $ Map.keys bagmap
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

day7p2 :: Map.Map String [(Int, String)] -> Int
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

-- December 9th
-- Checking numbers

day9p1 :: [Int] -> Int
day9p1 nums = helper preamble pairs remainder
  where
    preamble  = reverse $ take 25 nums
    remainder = drop 25 nums
    pairs     = [x+y | x <- preamble, y<- preamble]

    helper :: [Int] -> [Int] -> [Int] -> Int
    helper lst25 lstPs (x:xs)
      | x `elem` lstPs = helper lst25' lstPs' xs
      | otherwise      = x
        where
          lst24  = take 24 lst25
          lst25' = x:lst24
          lstPs' = (map(+x) lst24) ++ (take 600 lstPs)
-- answer = 731031916

-- Fast algorithm, viewing it as a set of runs to sum using prefix sum O(n^2).
day9p2 :: [Int] -> Int
day9p2 ls@(x:xs)
  | psm /= 0 = psm
  | otherwise = day9p2 xs
  where
    prefixsum :: [Int] -> [Int] -> Int -> Int
    prefixsum [] _ _ = 0
    prefixsum (i:is) vs sm
      | sm' == 731031916 = maximum vs' + minimum vs'
      | sm' > 731031916 = 0
      | otherwise = prefixsum is vs' sm'
      where
        sm' = i + sm
        vs' = i:vs

    psm = prefixsum ls [] 0

-- Slow algorithm, DFS traversal of a binary tree of sublists O(2^n).
day9p2' :: Int -> [Int] -> Int
day9p2' _ [_] = 0
day9p2' sm lst
  | sm < 731031916 = 0
  | sm == 731031916 = maximum lst + minimum lst
  | otherwise = if lsm /= 0 then lsm else rsm
    where
      (l:ls) = lst
      (r:rs) = reverse lst
      lsm = day9p2' (sm - l) ls
      rsm = day9p2' (sm - r) rs

runDay9 :: IO()
runDay9 = do
  print $ day9p1 day9data
  print $ day9p2 day9data

-- December 10th
-- Checking the charger

day10p1 :: [Int] -> Int
day10p1 js = (occur 3 ds +1) * (occur 1 ds +1)
  where
    fs = sort js
    ds = zipWith (-) (tail fs) fs

    occur :: Int -> [Int] -> Int
    occur = ((length .). filter) . (==)

fib3 :: [Integer]
fib3 = 1 : 1 : 2 : [fib3!!n + fib3!!(n+1) + fib3!!(n+2) | n <- [0..]]

day10p2 :: [Int] -> Integer
day10p2 = 
  product 
  . map ((fib3 !!) . length) 
  . filter (1 `elem`)
  . group 
  . (zipWith (-) =<< tail) 
  . sort
  . (0:)

-- Tree versions (much much slower)
day10p1' :: [Int] -> Int
day10p1' jlst= traverse 0 (0,0)
  where
    dvc = maximum jlst
    traverse :: Int -> (Int, Int) -> Int
    traverse c (three, one)
      | c == dvc = (three+1) * one
      | nxt - c == 3 = traverse nxt (three + 1, one)
      | nxt - c == 1 = traverse nxt (three, one + 1)
      | otherwise = traverse nxt (three, one)
        where
          nxt = minimum $ filter (>c) jlst

day10p2' :: [Int] -> Int
day10p2' js = traverse 0 fs
  where
    fs = sort js
    dvc = last fs

    traverse :: Int -> [Int] -> Int
    traverse c []
      | c == dvc = 1
      | otherwise = 0
    traverse c xs = sum $ map (\(o,os) -> traverse o os) $ getsubs (c+3) xs

    getsubs :: Int -> [Int] -> [(Int, [Int])]
    getsubs _ [] = []
    getsubs p (x:xs)
      | x <= p = (x,xs) : getsubs p xs
      | otherwise = []

runDay10 :: IO()
runDay10 = do
  print $ day10p1 day10data
  print $ day10p2 (0:day10data)


-- December 11th
-- Cellular Automata

type Key = (Int, Int)
type Val = Char
type Board = Map.Map Key Val
type Stepper = Board -> Key -> Val -> Val

rows, columns :: Int
rows = 98
columns = 89

printboard :: Board -> IO()
printboard b = 
  putStrLn 
  $ concat 
  $ intersperse "\n" 
  $ map (map (b Map.!)) 
  $ groupBy (\(r,_) (r',_) -> r == r') 
  $ sort 
  $ Map.keys b

genb :: [[Char]] -> Board
genb grid 
  = Map.fromList [((r,c), grid!!r!!c) | r <- [0..length grid - 1], c <- [0..length (grid!!0) -1]]

stepb :: Board -> Stepper -> Board
stepb b f 
  = Map.mapWithKey (f b) b

occup :: Stepper -> Board -> Int
occup f b
  | b == b' = (length $ filter (=='#') $ map (b Map.!) $ Map.keys b)
  | otherwise  = occup f b'
  where
    b' = stepb b f

day11p1 :: Stepper
day11p1 b (r,c) v
  | v == 'L' && '#' `notElem` advs                  = '#'
  | v == '#' && (length $ filter (=='#') advs) >= 4 = 'L'
  | otherwise                                       = v
  where
    advs = 
      map (b Map.!) 
      $ filter (\(r',c') -> (r' >= 0 && r'<= rows) && (c' >= 0 && c'<= columns)) 
      [(r-1,c),(r-1,c+1),(r,c+1),(r+1,c+1),(r+1,c),(r+1,c-1),(r,c-1),(r-1,c-1)]

day11p2 :: Stepper
day11p2 b (r,c) v
  | v == 'L' && seen == 0 = '#'
  | v == '#' && seen >= 5 = 'L'
  | otherwise             = v
  where
    seen = length . filter (==True) $ map (getline (r,c)) [(0,1),(1,1),(1,0),(1,-1),(0,-1),(-1,-1),(-1,0),(-1,1)]
    getline :: (Int,Int) -> (Int,Int) -> Bool
    getline (lr,lc) d@(dr,dc)
      | (0 > r' || r' > rows)    = False
      | (0 > c' || c' > columns) = False
      | b Map.! p' == '#'        = True
      | b Map.! p' == 'L'        = False
      | otherwise                = getline p' d
      where
        p'@(r',c') = (lr+dr,lc+dc)

runDay11 :: IO()
runDay11 = do
  print $ occup (day11p1) (genb day11data)
  print $ occup (day11p2) (genb day11data)

-- December 12th
-- Navigation by simple instructions.


-- Take east facing direction as 90:
-- N -> 0, E -> 90, S -> 180, W -> 270
day12p1 :: [(Char,Int)] -> Int
day12p1 ms = abs dx + abs dy
  where
    (dx,dy) = nav ms 90 (0,0)
    nav :: [(Char,Int)] -> Int -> (Int,Int) -> (Int,Int)
    nav [] _ p = p
    nav (m:ms) d p@(x,y) 
      = case m of
        ('N', v) -> nav ms d (x,y+v)
        ('S', v) -> nav ms d (x,y-v)
        ('E', v) -> nav ms d (x+v,y)
        ('W', v) -> nav ms d (x-v,y)
        ('L', v) -> nav ms ((d - v) `mod` 360) p
        ('R', v) -> nav ms ((d + v) `mod` 360) p
        ('F', v) -> 
          case d of
            0 -> nav ms d (x,y+v)
            90 -> nav ms d (x+v,y)
            180 -> nav ms d (x,y-v)
            270 -> nav ms d (x-v,y)

day12p2 :: [(Char,Int)] -> Int
day12p2 ms = abs dx + abs dy
  where
    (dx,dy) = nav ms  (10,1) (0,0)
    nav :: [(Char,Int)] -> (Int,Int) -> (Int,Int) -> (Int,Int)
    nav [] _ s = s
    nav (m:ms) p@(px,py) s@(sx,sy)
      = case m of
        ('N', v) -> nav ms (px,py+v) s
        ('S', v) -> nav ms (px,py-v) s
        ('E', v) -> nav ms (px+v,py) s
        ('W', v) -> nav ms (px-v,py) s
        ('F', v) -> nav ms p (sx + v*px, sy + v*py)
        ('L', 90) -> nav ms (-py,px) s
        ('L', 270) -> nav ms (py,-px) s
        ('R', 90) ->  nav ms (py,-px) s
        ('R', 270) -> nav ms (-py,px) s
        (_, 180) -> nav ms (-px,-py) s


testdata12 :: [(Char,Int)]
testdata12 = 
  [('F',10),
  ('N',3),
  ('F',7),
  ('R',90),
  ('F',11)]

-- December 13th
-- Taking the bus

day13p1 :: (Int, [String]) -> Int
day13p1 (start, buses) = wait * bus
  where
    bvals = map (\x -> read x :: Int) $ filter(/="x") buses
    (wait, bus) = minimum $ zip (map (\x -> x - (snd $ quotRem start x)) bvals) bvals

test :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
test (os, mb) (ind, tgt) = check 0
  where
    check :: Integer -> (Integer, Integer)
    check i
      | c `mod` tgt == 0 = (c, mb * tgt)
      | otherwise        = check $ i + 1 
        where
          c = os + i * mb + ind

developlist :: [String] -> [(Integer, Integer)]
developlist [] = []
developlist bs = (toInteger (length xs) + 1, read n :: Integer):developlist ns
  where
    xs = takeWhile (=="x") bs
    (n:ns) = dropWhile  (=="x") bs

day13p2 :: (Int, [String]) -> Integer
day13p2 (_,b) = fst (foldl test (0,s) ns) - toInteger(length b- 1)
  where
    ((_,s):ns) = developlist b

runDay13 :: IO()
runDay13 = do
  print $ day13p1 day13data
  print $ day13p2 day13data

testdata13 :: (Int, [String])
testdata13 = 
  (939,
  ["7","13","x","x","59","x","31","19"])

--december 14th
-- bit masks
day14p1 :: [BitStruction] -> Int
day14p1 j = (sum.map snd) rslt
  where
    (rslt, _, _) = foldl' run ([],[],[]) j

    run :: ([(Int,Int)], [Int], [Int]) -> BitStruction -> ([(Int,Int)], [Int], [Int])
    run (v, _, _) (Mask m ) = (v, ones, zeros)
      where
        m' = reverse m
        ones = map snd (filter ((=='1').fst) $ zip m' [0..])
        zeros = map snd (filter ((=='0').fst) $ zip m' [0..])

    run (v, os, zs) (Load ads val) = (insert (ads,v') v, os, zs )
      where
        v' = foldl clearBit (foldl setBit val os) zs
    
    insert :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
    insert v@(ads, _) vs = v:filter((/=ads).fst) vs

day14p2 :: [BitStruction] -> Int
day14p2 is = Map.foldr (+) 0 $ snd $ foldl' next ("",Map.fromList []) is
  where
    next :: (String, Map.Map Int Int) -> BitStruction -> (String, Map.Map Int Int)
    next (_, mp) (Mask m') = (m', mp)
    next (m, mp) (Load ad val) = (m, foldl' (\m k -> Map.insert k val m) mp (genads m ad))
    
    genads :: String -> Int -> [Int]
    genads m ad = ad''
      where
        m' = reverse m
        ones = map snd (filter ((=='1').fst) $ zip m' [0..])
        misses = map snd (filter ((=='X').fst) $ zip m' [0..])
        ad' = foldl' setBit ad ones
        ad'' = foldl' (\ads' b -> map (`setBit` b) ads' ++ map (`clearBit` b) ads') [ad'] misses

-- Broekn version (non functional)
day14p2' :: [BitStruction] -> Int
day14p2' is = sum $ map (\(x,y) -> 2^((length.filter (=='X')) x) * y) fin
  where 
    cd = compile is ""
    fin = foldl' filterins [] cd

filterins :: [(String, Int)] -> (String, Int) -> [(String, Int)]
filterins ls e@(ads,_)
  = e:filter (\(ads',_) -> elem False (zipWith (\x y -> case x of 'X' -> True;_ -> x == y) ads ads')) ls

compile :: [BitStruction] -> String -> [(String, Int)]
compile [] _ = []
compile ((Mask m):is) _ = compile is m
compile ((Load ads val):is) m 
  = (zipWith (\x y -> case x of '0' -> y;'1' -> '1';'X' -> 'X') m (showBitVector ads masklen), val) : compile is m

runDay14 :: IO()
runDay14 = do
  print $ day14p1 day14data  
  print $ day14p2 day14data  

-- showBitVector aken for mock test spec
showBitVector :: Int -> Int -> String
showBitVector _ 0 = ""
showBitVector bv n = showBitVector (bv `div` 2) (n - 1) ++ show (bv `mod` 2)

masklen :: Int
masklen = 36

testdata14 :: [BitStruction]
testdata14 = 
  [Mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
  Load 8 11,
  Load 7 101,
  Load 8 0]

testdata14' :: [BitStruction]
testdata14' = 
  [Mask "000000000000000000000000000000X1001X",
  Load 42 100,
  Mask "00000000000000000000000000000000X0XX",
  Load 26 1]


-- December 15th
-- Elf number game
day15 :: Int -> [Int] -> Int
day15 n ls = game (length ls) (last ls) (Map.fromList $ zip ls [1..])
  where
    game :: Int -> Int -> Map.Map Int Int -> Int
    game ps pv mp
      | ps == n = pv
      | otherwise 
          = case Map.lookup pv mp of
            Just s -> game (ps+1) (ps-s) (Map.insert pv ps mp)
            Nothing -> game (ps+1) 0 (Map.insert pv ps mp)

runDay15 :: IO()
runDay15 = do
  print $ day15 2020 day15data
  print $ day15 30000000 day15data

main :: IO ()
main = do
  runDay14