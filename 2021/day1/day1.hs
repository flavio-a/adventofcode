import ReadFile

main = do
    content <- ReadFile.readFileArg
    let numbers = map read $ lines content :: [Int]
    let res1 = length $ filter (uncurry (>)) $ zip (drop 1 numbers) numbers
    putStrLn $ show res1
    let res2 = length $ filter (uncurry (>)) $ zip (drop 3 numbers) numbers
    putStrLn $ show res2
