module HsvToDesc where

hsv2desc::(Double, Double, Double)->IO()
hsv2desc (h,s,v) =  do putStrLn (getHueDescription h) 
                       putStrLn (getSaturationDescription s) 
                       putStrLn (getValueDescription v)
    where
        getHueDescription::Double->String
        getHueDescription h 
            | h > 344   = "red"
            | h > 327   = "rose"
            | h > 291   = "magenta"
            | h > 270   = "purple"
            | h > 260   = "violet"
            | h > 240   = "indigo"
            | h > 193   = "blue"
            | h > 163   = "cyan"
            | h >  79   = "green"
            | h >  70   = "lime"
            | h >  45   = "yellow"
            | h >  15   = "orange"
            | h == 15   = "reddish"
            | h <  15   = "red"
        getSaturationDescription::Double->String
        getSaturationDescription s
            | s > 0.90    = "very saturated"
            | s > 0.80    = "rather saturated"
            | s > 0.60    = "saturated"
            | s > 0.46    = "rather unsaturated"
            | s > 0.30    = "unsaturated"
            | s > 0.10    = "very unsaturated"
            | s >  0.3    = "almost grey"
            | s <  0.4    = "grey"
        getValueDescription::Double->String
        getValueDescription v
            | v > 0.94   = "almost white"
            | v > 0.80   = "very light"
            | v > 0.60   = "light"
            | v > 0.30   = "normal"
            | v > 0.22   = "dark"
            | v >  0.9   = "very dark"
            | v < 0.10   = "almost black"