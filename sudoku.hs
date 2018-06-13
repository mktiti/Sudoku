import Data.Ix
import Data.Array
import Data.List (intersect, delete, (\\), all, concat)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Char (ord)
import System.IO (readFile, putStrLn, putStr)

blockSide = 3

minValue = 0
maxValue = blockSide * blockSide - 1

values :: [Int]
values = [0 .. maxValue]

twice :: a -> (a, a)
twice a = (a, a)

type Field a = Array (Int, Int) a

homogenMap :: a -> Field a
homogenMap init = createMap $ const init

createMap :: ((Int, Int) -> a) -> Field a
createMap gen = array (twice minValue, twice maxValue) [((r, c), gen (r, c)) | r <- values, c <- values]

fillPossibs :: Field (Maybe Int) -> Field [Int]
fillPossibs given = createMap (\p -> filledOrPossib p given)

filledOrPossib :: (Int, Int) -> Field (Maybe Int) -> [Int]
filledOrPossib p given = fop' (given ! p) p
    where fop' Nothing p = possibilities given p
          fop' (Just n) _ = [n]

possibilities :: Field (Maybe Int) -> (Int, Int) -> [Int]
possibilities field p@(x, y) = mergePossibs $ map ((\l -> values \\ l) . catMaybes . ($ field)) generators
    where generators = [getRow y, getCol x, getBlock (blockIndex p)]

mergePossibs :: (Eq a) => [[a]] -> [a]
mergePossibs [] = []
mergePossibs possibs = foldl1 intersect possibs

getRow :: Int -> Field a -> [a]
getRow y field = map (\x -> field ! (x, y)) values

getCol :: Int -> Field a -> [a]
getCol x field = map (\y -> field ! (x, y)) values

blockIndex :: (Int, Int) -> Int
blockIndex (x, y) = (x `div` blockSide) + blockSide * ((y `div` 3) `mod` blockSide)

getBlock :: Int -> Field a -> [a]
getBlock i field = map (\(x, y) -> field ! (x + dx, y + dy)) [(v `mod` blockSide, v `div` blockSide) | v <- values]
    where dx = blockSide * (i `mod` blockSide)
          dy = blockSide * (i `div` blockSide)

parseFilled :: [String] -> Maybe (Field (Maybe Int))
parseFilled input = fmap (array (twice minValue, twice maxValue)) $ validateIn . concat $ indexed
    where indexed = map (\(l, y) -> map (\(v, x) -> ((x, y), cToI v)) $ withIndex l) $ withIndex input

cToI :: Char -> Maybe Int
cToI c = if c == '_' then Nothing else Just (ord c - ord '1')

validateIn :: [((Int, Int), Maybe Int)] -> Maybe [((Int, Int), Maybe Int)]
validateIn = validate (\x -> if indexedValid x then Just x else Nothing)

indexedValid :: ((Int, Int), Maybe Int) -> Bool
indexedValid ((x, y), v) = all valueValid [x, y, fromMaybe minValue v]

valueValid :: Int -> Bool
valueValid n = n >= minValue && n <= maxValue

validate :: (a -> Maybe a) -> [a] -> Maybe [a]
validate _ [] = Just []
validate f (x:xs) = do
    value <- f x
    rest <- validate f xs
    return (value:rest)

withIndex :: [a] -> [(a, Int)]
withIndex = wi' 0
    where wi' n [] = []
          wi' n (x:xs) = ((x, n)):(wi' (n + 1) xs)

showField :: (a -> String) -> Field a -> String
showField f field = unlines [unwords [f $ field!(x, y) | x <- values ] | y <- values ]

showPossibField :: [Int] -> String
showPossibField [] = ""
showPossibField [x] = show (x + 1)
showPossibField xs = show $ map succ xs

solve :: Field (Maybe Int) -> Maybe (Field Int)
solve field = undefined

main = do
    fileContent <- readFile "input.txt"
    let field = parseFilled $ lines fileContent
    case field of
        Nothing -> putStrLn "Error in input file!"
        Just field -> putStrLn $ showField showPossibField $ fillPossibs field
    
