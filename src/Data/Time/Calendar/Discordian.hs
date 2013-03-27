module Data.Time.Calendar.Discordian 
       ( toDiscordian, fromDiscordian, fromDiscordianValid, stTibsDay
       , showSeason, showDayOfWeek, ddate)   
       where
import Data.Time.Calendar
import Data.Time.Calendar.MonthDay

-- |Converts a modified Julian date to a Discordian date
--  
--  The result format is either (year, 'Just' (season, day))
--  for normal days or (year, 'Nothing') for St. Tibs Day
toDiscordian :: Day -> (Integer, Maybe (Int, Int))
toDiscordian day = (dYear, dDayAndMonth)
  where
    (gYear, gMonth, gDay) = toGregorian day
    dYear = gYear + 1166
    dayOfYear = monthAndDayToDayOfYear False gMonth gDay
    (dMonth, dDay) = dayOfYear `divMod` 73
    dDayAndMonth
      | isLeapYear gYear && gMonth == 2 && gDay == 29 = Nothing
      | otherwise = Just (dMonth+1, dDay)

-- |Converts the Discordian date to the modified Julian date, clipping values
--  to valid ranges.
--  
--  Because St. Tibs Day is not considered part of the Discordian week, you
--  should use the 'stTibsDay' function to calculate those days.
fromDiscordian :: Integer -> Int -> Int -> Day
fromDiscordian dYear dMonth dDay = fromGregorian gYear gMonth gDay
  where
    dMonth'= clip 1 5 dMonth
    dDay' = clip 1 73 dDay
    gYear = dYear - 1166
    dayOfYear = (dMonth'-1) * 73 + dDay'
    (gMonth, gDay) = dayOfYearToMonthAndDay False dayOfYear

-- |Similar to 'fromDiscordian', but invalid dates result in 'Nothing'
fromDiscordianValid :: Integer -> Int -> Int -> Maybe Day
fromDiscordianValid dYear dMonth dDay = do
  dMonth' <- clipValid 1 5 dMonth
  dDay' <- clipValid 1 73 dDay
  let gYear = dYear - 1166
      dayOfYear = (dMonth' - 1) * 73 + dDay'
      (gMonth, gDay) = dayOfYearToMonthAndDay False dayOfYear
  fromGregorianValid gYear gMonth gDay
  
-- |Converts the St. Tibs Day of a given year to its equivalent modified Julian date. A value of 'Nothing' indicates no St. Tibs Day for the given year.
stTibsDay :: Integer -> Maybe Day
stTibsDay dYear
  | isLeapYear gYear = Just (fromGregorian gYear 2 29)
  | otherwise = Nothing
    where
      gYear = dYear - 1166

-- |Show the name of a Discordian season. Input values are clipped to valid ranges.
showSeason :: Int -> String
showSeason s = case clip 1 5 s of
  1 -> "Chaos"
  2 -> "Discord"
  3 -> "Confusion"
  4 -> "Bureaucracy"
  5 -> "The Aftermath"
  x -> error $ "showSeason: impossible season " ++ show x

-- |Given the season and day, show the name of the day of the week.
--  Input values are clipped to valid ranges.
showDayOfWeek :: Int -> Int -> String
showDayOfWeek month day = 
  case ((month' - 1) * 73 + day') `mod` 5 of
    0 -> "Setting Orange"
    1 -> "Sweetmorn"
    2 -> "Boomtime"
    3 -> "Pungenday"
    4 -> "Prickle-Prickle"
    x -> error $ "showDayOfWeek: impossible mod result " ++ show x
  where
    month' = clip 1 5 month
    day' = clip 1 73 day

-- |A function that emulates the default behavior of the 
--  ddate command-line utility
ddate :: Day -> String
ddate jDay = monthDayStr ++ yearStr
  where
    (year, mMonthDay) = toDiscordian jDay
    monthDayStr = case mMonthDay of
      Nothing -> "St. Tibs Day"
      Just (month, day) -> 
        showDayOfWeek month day ++ ", " 
        ++ show day ++ th day ++ " of " ++ showSeason month
    yearStr = " in the YOLD " ++ show year
    
    -- yay English!
    th n = let s = show n
           in case head s of
             '1' -> "th"
             _ -> case last s of
               '1' -> "st"
               '2' -> "nd"
               '3' -> "rd"
               _   -> "th"
            

-- utility functions

clip :: Int -> Int -> Int -> Int
clip x y i
  | i < x = x 
  | i > y = y
  | otherwise = i
                
clipValid :: Int -> Int -> Int -> Maybe Int
clipValid x y i
  | i >= x && i <= y = Just i
  | otherwise = Nothing
