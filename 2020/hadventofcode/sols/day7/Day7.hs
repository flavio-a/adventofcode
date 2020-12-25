import ReadFile
import Data.List.Split
import Data.List
import Data.Maybe

type Color = (String, String)
type Content = (Color, Int)
type Node = (Color, [Content])

parseLine :: String -> Node
parseLine line = (color, contents)
    where
        -- Take exactly 2 Strings, expecting them to be like "bright white"
        parseColor :: [String] -> Color
        parseColor [s1, s2] = (s1, s2)
        -- Take exactly 4 Strings, expecting them to be like "3 bright white bags"
        parseContent :: [String] -> Content
        parseContent [s1, s2, s3, _] = ((s2, s3), read s1)

        tokens = words line
        color = parseColor $ takeWhile (/= "bags") $ tokens
        -- The two tails drop "bags" and "contain"
        rest = tail $ tail $ dropWhile (/= "bags") $ tokens
        contents :: [Content]
        contents = if length(rest) == 3
                   then []
                   else map parseContent $ chunksOf 4 rest

reverseGraph :: [Node] -> [Node]
reverseGraph g = g'
    where
        colors = map fst g
        g' = [(src, [(dest, w)
                     | (dest, edges) <- g, (src', w) <- edges, src' == src ])
              | src <- colors]

getParents :: [Node] -> Color -> [Color]
getParents g color = map fst $ concatMap snd $ maybeToList node
    where
        node = find ((== color) . fst) g :: Maybe Node

-- Ugly solution
getAncestors :: [Node] -> [Color] -> Color -> [Color]
getAncestors g alreadyseen color = parents ++ grandparents
    where
        notSeen :: Color -> Bool
        notSeen = not . flip elem alreadyseen

        parents = filter notSeen $ getParents g color :: [Color]

        grandparents :: [Color]
        grandparents = concatMap (getAncestors g (parents ++ alreadyseen)) parents

getChildrenCount :: [Node] -> Color -> (Int, [Content])
getChildrenCount g color = (sum $ map snd $ snd node, snd node)
    where
        node = fromJust $ find ((== color) . fst) g :: Node

-- This is ugly too
getTotal :: [Node] -> Color -> Int
getTotal g color = val + (sum [ weight * (getTotal g color)
                              | (color, weight) <- contents
                              ])
    where
        (val, contents) = getChildrenCount g color

targetColor = ("shiny", "gold")

main = do
    content <- ReadFile.readFileArg
    let graph = map parseLine $ lines content
    let revgraph = reverseGraph graph
    let ancestors = nub $ getAncestors revgraph [] targetColor
    putStrLn $ show $ length ancestors
    putStrLn $ show $ getTotal graph targetColor
