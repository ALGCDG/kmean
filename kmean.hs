--kmean k points
--    | k <= 0 = 0
--    | k == 0 = replace k 0
--    | map (classify (update_centroids (Random.Sample points) points 1000)) points

distance :: [Float] -> [Float] -> Float
distance v1 v2 = sqrt (sum (map hyp (zip v1 v2)))
    where
        square x = x ** 2
        sub x = fst x - snd x
        hyp x = square (sub x)

closest :: [Float] -> [([Float], Int)] -> (Float, Int)
closest point classifications 
    | length classifications == 1 = (distance point (fst(head classifications)), 0)  
    | otherwise = if fst x < fst y then x else y
    where
        x = (distance point (fst(head classifications)), snd(head classifications))
        y = closest point (tail classifications)

enumerate :: [a] -> [(a, Int)]
enumerate list = zip list [0..]

classify :: [[Float]] -> [Float] -> ([Float], Int)
classify centroids point = (point, snd(closest point (enumerate(centroids))))





vector_sum :: [Float] -> [Float] -> [Float]
vector_sum a b = map (\x -> fst x + snd x) pairs
    where pairs = zip a b

list_vector_sum :: [[Float]] -> [Float]
list_vector_sum vl
    | length vl == 2 = vector_sum (head vl) (head(tail vl))
    | otherwise = vector_sum (head vl) (list_vector_sum (tail vl))

avg_vector :: [[Float]] -> [Float]
avg_vector vl = map (\x -> x/(fromIntegral(length vl)))  (list_vector_sum(vl))

update_centroids :: [[Float]] -> [[Float]] -> Int -> [[Float]]
update_centroids centroids points limit
    | limit == 0 = centroids
    | otherwise = update_centroids new_centroids points (limit-1)
    where
        point_class = (map (classify centroids) points)
        new_centroids = map(\n -> avg_vector(map(\p -> fst p)(filter(\p -> snd p == n)(point_class)))) [0..((length centroids) - 1)]

