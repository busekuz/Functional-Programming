module RgbToHsv where
    

import Data.Fixed
rgb2hsv::(Integer, Integer, Integer)->(Double, Double, Double)
rgb2hsv (r, g, b) = (hue, saturation, value)
    where
        r' = fromIntegral(r) / 255
        g' = fromIntegral(g) / 255
        b' = fromIntegral(b) / 255 
        cMax = getMaximum (r',g',b')
        cMin = getMinimum (r',g',b')
        delta = cMax - cMin
        value = cMax
        saturation = if cMax == 0 then 0 else (delta / cMax)
        hue::Double
        hue 
            |delta == 0   = 0.0
            |cMax == r'   = 60 * (mod' ((g' -b')/delta) 6)
            |cMax == g'   = 60 * (((b' - r')/delta) + 2)
            |cMax == b'   = 60 * (((r' - g')/delta) + 4)
            |otherwise    = error "Incorrect input for calculation of hue."                                        
        --Returns maximum of a (r, g, b) value. 
        getMaximum::(Double, Double, Double)->Double
        getMaximum (a, b, c)
            |(max a b) <= c = c
            |a <= b         = b
            |otherwise      = a

        --Returns minimum of a (r, g, b) value. 
        getMinimum::(Double, Double,Double)->Double
        getMinimum (a, b, c)
            |(min a b) >= c = c
            |b <= a         = b
            |otherwise      = a