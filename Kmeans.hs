module Kmeans
where

import Data.List
import Data.Function (on)

import Point
import System.Random
import System.Random.Shuffle

import UniformList


----Examples---------------------------------------------
---------------------------------------------------------


n=30
m=20
iters =6

examples  :: [[Point Float]]
examples  =     [
                 [ [2,5], [1,1],[1,5],[10,10],[15,15],[10,15]],
                 [ [fromIntegral x,fromIntegral y] | x<-[0..n-1],y<-[0..m-1], x^2+y^2<60 ],
                 [ [fromIntegral x,fromIntegral y] | x<-[0..n-1],y<-[0..m-1], simplexOr x y  ]
                ]

simplexOr :: Int -> Int -> Bool
simplexOr x y = or [
                        x>3 && x<=8 && y>4 && y<=7,
                        x>15 && x<=17 && y>4 && y<=7,
                        x+2*y-45>0 && x+2*y-55<0 && x-y-5>0 && x-y-10<=0,
                        x+2*y-40>0 && x+2*y-50<0 && x-y+5>0 && x-y<=0
                    ]

showMultipleExamples gen =
                    concat $
                    map (\(i,example) ->
                            concat $
                            map (\k ->
                                    "example " ++ show i ++ ", iteration "++show iters++"(k="++show k++")\n" ++
                                    let example' = shuffle' example (length example) gen  
                                    in displayLabeled n m  (zip example' (kmeans iters k example'))
                                ) [2,3,4]
                        )      
                        $zip [1..] examples


showMultipleIterations example =
                    concat $
                    ["multiple itarations on one example\n"]++
                    map (\iter ->
                            "example 2, iteration " ++ show iter ++ "(k=4)\n" ++
                            displayLabeled n m  (zip example (kmeans iter 4 example)) 
                        )
                        [0..iters]
main :: IO()
main = do
        gen<-newStdGen
        let example=examples!!2;
        --putStrLn $ showMultipleIterations $ shuffle' example (length example) gen 
        putStrLn $ showMultipleExamples gen
        


----Definition-------------------------------------------
---------------------------------------------------------

means :: [Point Float] -> [Int] -> [Point Float]
means xs cs = map meanOverFsts $  groupBy ((==) `on` snd) $   sortBy (compare `on` snd) $     zip xs cs
    where
    meanOverFsts ps = mean $ fst $ unzip ps 



kmeans :: Int -> Int ->[Point Float] -> [Int]
kmeans iters k xs = iteration iters $ uniformList k (length xs)
    where 
    meansXs         =   means xs
    iteration 0 cs  =   cs
    iteration i cs  =   iteration (i-1) $   map (\x -> fst $    minimumBy (compare `on` snd) $ 
                                                                zip [1..k] $ map (dist x) (meansXs cs))
                                            xs


