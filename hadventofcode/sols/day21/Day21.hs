import ReadFile
import Data.List
import Data.Maybe
import Data.Function (on)
import Data.Tuple.Extra (second)

type Ingredient = String
type Allergene = String
type Parings = ([Ingredient], [Allergene])

parseLine :: String -> Parings
parseLine line = (sort $ words ingrs, alls')
    where
        (ingrs:alls:_) = splitOneOf "()" line
        alls' = filter (/= "") $ tail $ splitOneOf " ," alls

getAllList :: Allergene -> [Parings] -> [Ingredient]
getAllList allerg = foldl1' intersect . map fst . filter ((allerg `elem`) . snd)

-- This should be recursive...
matchAllerg :: [Allergene] -> [(Allergene, [Ingredient])] -> [(Allergene, Ingredient)]
matchAllerg [] _ = []
matchAllerg allAllergs allergPoss = firstPair:matchAllerg (tail allAllergs') allergPoss'
    where
        allAllergs' = sortBy (compare `on` (length . getIngrs)) allAllergs
        firstPair = (head allAllergs', head $ getIngrs $ head allAllergs')
        allergPoss' = filter ((/= 0) . length . snd) $ map (second $ filter (/= snd firstPair)) allergPoss

        getIngrs :: Allergene -> [Ingredient]
        getIngrs = concat . maybeToList . flip lookup allergPoss

main = do
    content <- ReadFile.readFileArg
    let allPairings = map parseLine $ lines content :: [Parings]
    let allIngredients = nub $ concatMap fst allPairings
    let allAllergenes = sort $ nub $ concatMap snd allPairings
    -- Part 1
    let allergPoss = map (\a -> (a , getAllList a allPairings)) allAllergenes
    let noAllIngrs = filter (not . (`elem` concatMap snd allergPoss)) allIngredients
    putStrLn $ show $ length $ filter (`elem` noAllIngrs) $ concatMap fst allPairings
    -- Part 2
    let matchings = matchAllerg allAllergenes allergPoss
    putStrLn $ intercalate "," $ map snd $ sort matchings
