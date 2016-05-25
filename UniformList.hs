module UniformList(uniformList,uniformListExample)
where

----Description------------------------------------------
---------------------------------------------------------
--  L  =  uniformList m k = [ 1...1  , 2...2 , ...k times... , k...k ]
--  length L = m


----Examples---------------------------------------------
---------------------------------------------------------

test = map (\p-> (p,uncurry uniformList p)) $ concat $ map (\m -> map (\k -> (k,m)) [1..6]) [20,23]  
uniformListExample =concat $ map ((++"\n") . show) test

main :: IO()
main = do putStrLn uniformListExample


----Definition-------------------------------------------
---------------------------------------------------------

uniformList :: Int -> Int -> [Int]
uniformList k m =   concat $ zipWith (++) rest phase1 ++ (reverse . take (k-restLen) . reverse) phase1
    where 
    partLen = div m k
    restLen = mod m k
    rest    = map (:[]) [1..restLen]
    phase1  = map (take partLen . repeat) [1..k]



