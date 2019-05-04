{-# LANGUAGE ParallelListComp #-}

module HsvGradient where


hsvGradient::(Double, Double, Double)->(Double, Double, Double)->Int->[(Double, Double, Double)]
hsvGradient (h1, s1, v1) (h2, s2, v2) step = [(h, s, v) | h <- [h1, h1+stepH..h2] | s <- [s1,s1+stepS..s2] | v <- [v1,v1+stepV..v2]]
    where
        stepH = (h2 - h1) / fromIntegral(step)
        stepS = (s2 - s1) / fromIntegral(step)
        stepV = (v2 - v1) / fromIntegral(step)

