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

classify :: [[Float]] -> [Float] -> Int
classify centroids point = snd(closest point (enumerate(centroids)))

--update_centroids centroids points limit
--    | limit == 0 = centroids
--    | otherwise = update_centroids (map (classify centroids) points) points limit-1
