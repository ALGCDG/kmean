import System.Random
import Data.Bifunctor (first)

enumerate = flip zip [0..]

kmean :: StdGen -> Int -> [[Float]] -> [Int]
kmean seed k points
    | k <= 0 = [-1]
    | k == 1 = replicate k 0
    | otherwise = map (snd . classify (update_centroids samples points 1000)) points
    where
        l = pred . length $ points
        index :: [Int]
        index = take k $ randomRs (0, l) seed
        samples = map (points !!) index

distance :: [Float] -> [Float] -> Float
distance v1 v2 = sqrt (sum (map hyp (zip v1 v2)))
    where
        square = (** 2)
        sub = uncurry (-)
        hyp = square . sub

closest :: [Float] -> [([Float], Int)] -> (Float, Int)
closest point [c] = first (distance point) c -- pattern matches for when 2nd arg is list with single element
closest point (c:cs) = min x y
    where
        x = first (distance point) c
        y = closest point cs -- do we need $???

classify :: [[Float]] -> [Float] -> ([Float], Int)
classify centroids point = (point, snd(closest point (enumerate(centroids))))

vector_sum :: [Float] -> [Float] -> [Float]
vector_sum = zipWith (+)

list_vector_sum :: [[Float]] -> [Float]
list_vector_sum = foldl1 vector_sum

avg_vector :: [[Float]] -> [Float]
avgVector = map . flip (/) . fromIntegral . length <*> listVectorSum

update_centroids :: [[Float]] -> [[Float]] -> Int -> [[Float]]
update_centroids centroids points limit
    | limit == 0 = centroids
    | otherwise = update_centroids new_centroids points (limit-1)
    where
        point_class = map (classify centroids) points
        classifications = [0..(pred . length $ centroids)]
        new_centroids = map (\n -> avg_vector(map (\p -> fst p) (filter (\p -> snd p == n) (point_class)))) classifications
        --newCentroids = map (avgVector . map fst . flip filter pointClass . (. snd) . (==)) classifications

