module Main where
    
import NameToRgb
import HsvToRgb
import HsvGradient
import RgbToHsv 

main = do   
        firstColor <- getLine
        lastColor  <- getLine
        step       <- getLine
        let firstRGB   = name2rgb firstColor
        let lastRGB    = name2rgb lastColor
        let firstHSV   = rgb2hsv firstRGB
        let lastHSV    = rgb2hsv lastRGB
        let gradients  = hsvGradient firstHSV lastHSV (read step)
        putStrLn (show gradients) 



        