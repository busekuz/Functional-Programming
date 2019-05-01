module HsvToRgb where
    
import Data.Fixed
hsv2rgb::(Double, Double, Double)->(Double, Double, Double)
hsv2rgb (h, s, v)
    |h >=0 || h < 60        = getRGB (c, x, 0) m
    |h >=60 || h < 120      = getRGB (x, c, 0) m
    |h >=120 || h < 180     = getRGB (0, c, x) m
    |h >=180 || h < 240     = getRGB (0, x, c) m
    |h >=240 || h < 300     = getRGB (x, 0, c) m
    |h >=300 || h < 360     = getRGB (c, 0, x) m
    where
        c = v * s
        x = c * (1 - abs ((mod' (h / 60) 2) - 1))
        m = v - c 
        getRGB::(Double, Double, Double)->Double->(Double, Double, Double)
        getRGB (r', g', b') m = ((r' + m) * 255, (g' + m) * 255, (b' + m) * 255)