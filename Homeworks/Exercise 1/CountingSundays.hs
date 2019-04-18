module CountingSundays where 

-- Part 1
--Returns day of week  0 = Saturday, 1 = Sunday, ..., 6 = Friday) from given date (yyyy mm dd)
dayOfWeek::Integer -> Integer -> Integer -> Integer
dayOfWeek year month day = (day + t + k + (k `div` 4) + (divideFour j) + (5 * j)) `mod` 7
    where
        m = if month < 3 then month + 12 else month
        y = if month < 3 then year - 1 else year
        t = floor (fromIntegral (13 * (m + 1)) / 5.0)
        k = y `mod` 100
        j = y `div` 100
        divideFour::Integer->Integer
        divideFour n = n `div`4 


--Part 2

--sundays  calculates the number of months that starts with sunday between given years
sundays1::Integer -> Integer -> Integer
sundays1 start end = sundays' start 1
 where
  sundays' :: Integer -> Integer -> Integer
  sundays' y m
   | y > end = 0
   | otherwise = if dayOfWeek y m 1 == 1 then (rest + 1) else rest
   where
    nextY = if (m == 12) then (y + 1) else y
    nextM = if (m == 12) then 1 else (m+1)
    rest = sundays' nextY nextM


--sunday  eliminates rest variable of sundays function
sunday::Integer -> Integer -> Integer
sunday start end = sundays' start 1
 where
  sundays' :: Integer -> Integer -> Integer
  sundays' y m
   | y > end = 0
   | otherwise = if dayOfWeek y m 1 == 1 then sundays' nextY nextM + 1 else sundays' nextY nextM
   where
    nextY = if (m == 12) then (y + 1) else y
    nextM = if (m == 12) then 1 else (m+1)


-- Part 3
--sundaysTailRecursive is a tail recursive version of sundays function (sundays1)
sundaysTailRecursive::Integer -> Integer -> Integer
sundaysTailRecursive start end = sundays' start 1 0
 where
  sundays' :: Integer -> Integer -> Integer -> Integer
  sundays' y m acc
   | y > end = acc
   | otherwise = if dayOfWeek y m 1 == 1 then sundays' nextY nextM (acc + 1) else sundays' nextY nextM acc
   where
    nextY = if (m == 12) then (y + 1) else y
    nextM = if (m == 12) then 1 else (m+1)



--Part 4 

daysInMonth::Integer->Integer->Integer
daysInMonth m y
    | (m == 2)                                          = if (leap y) then 29 else 28
    | (m == 4) || (m == 6) || (m == 9) || (m == 11)     = 30
    | otherwise                                         = 31
    where       
        leap::Integer->Bool
        leap y = (y `mod` 4 == 0) && (y `mod` 100 /= 0) || (y `mod` 400 == 0)



sundays2::Integer->Integer->Integer
sundays2 start end = sundays' start 1 0 (((dayOfWeek start 1 1) - 1) `mod` 7)
    where
        sundays'::Integer->Integer->Integer->Integer->Integer
        sundays' year month n weekday 
            | year > end                             = n
            |( weekday + (daysInMonth month year `mod` 7)) `mod` 7 == 0   = sundays' nextY nextM (n + 1) (weekday + ((daysInMonth month year) `mod` 7))
            | otherwise                                                   = sundays' nextY nextM n (weekday + ((daysInMonth month year) `mod` 7))
            where
                nextY = if (month == 12) then (year + 1) else year
                nextM = if (month == 12) then 1 else (month+1) 



