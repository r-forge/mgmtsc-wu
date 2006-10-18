
# Examples from the Monograph:
#   "Rmetrics - Financial Engineering and Computational Finance"
#     written by Diethelm Wuertz, to be published
#   Chapter 2.3 - Rmetrics 'timeDate' Objects
#   Chapter 2.4 - Rmetrics 'timeSeries' Objects
#   Chapter 2.5 - Rmetrics Holiday Calendars

# Note Chapter 2 of the Monograph is documented for R 2.2, here we use R2.4


################################################################################
# Chapter 2.2 - Functions and methods for 'timeDate' objects


### Load Packages:


    require(fCalendar)
    
    
# ------------------------------------------------------------------------------
    

### Example 2.3.1: Financial Center NewYork


    # DST Rules for New York in the Eighties:
    NewYork()[124:139, ]

    # Create a Financial Center named Eastern and Pacific:
    Eastern = NewYork
    Pacific = LosAngeles
    
 
# ------------------------------------------------------------------------------


### Example 2.3.2: Create 'timeDate' Character Vectors

  
    # Create Date and Time Strings:
    dts = c("1989-09-28","2001-01-15","2004-08-30","1990-02-09")
    dts
    tms = c(  "23:12:55",  "10:34:02",  "08:30:00",  "11:18:23")
    tms
    td = paste(dts, tms)
    td

    # Create 'timeDate' Object: 
    # GMT Dates:
    GMT = timeDate(td)
    print(GMT)
    # For Zurich we have:
    ZURICH = timeDate(td, FinCenter = "Zurich")
    print(ZURICH)
    ZURICH = timeDate(td, zone = "Zurich", FinCenter = "Zurich")
    print(ZURICH)
    
    
# ------------------------------------------------------------------------------


### Example 2.3.3: Display the Internal Representation


    # HOMEWORK:
    # timeDate - Display the Internal Representation:
    # NOTE: internal Representation has changed with 
    #   R 2.3.1 from POSIXlt to POSIXct
    # What class?
    class(ZURICH)    
    unclass(ZURICH)  
    
    
# ------------------------------------------------------------------------------


### Example 2.3.4: Display the '@Data' Slot
    

    # HOMEWORK:
    # timeDate - Display the Internal Representation for the @Data Slot:  
    # What class?
    class(GMT@Data)    
    unclass(GMT@Data)  
    
    
# ------------------------------------------------------------------------------


### Example 2.3.5: Create 'timeCalender' Vectors


    # time Calendar -
    # Show Argument List:
    args(timeCalendar)
    
    
    # Create 'timeDate' object with 'timeCalendar':
    CCYYMMDD = timeCalendar(
        m = c(9, 1, 8, 2),
        d = c(28, 15, 30, 9),
        y = c(1989, 2001, 2004, 1990), FinCenter = "GMT")
    print(CCYYMMDD)
  
    
    # HOMEWORK:
    # timeCalender - Continued ...
    CCYYMMDD = timeCalendar(
        m = c(9, 1, 8, 2),
        d = c(28, 15, 30, 9),
        y = c(1989, 2001, 2004, 1990), FinCenter = "Zurich")
    print(CCYYMMDD)

    
    # HOMEWORK:
    # timeCalender - Continued ...
    CCYYMMDDhhmmss = timeCalendar(
        h = c(9, 14), min = c(15, 23))
    print(CCYYMMDDhhmmss)
    
    
# ------------------------------------------------------------------------------


### Example 2.3.6: 'timeSequence' Function


    # timeSequence -
    # Show Argument List:
    args(timeSequence)
   
    
    # GMT/Zurich Daily:
    tS = timeSequence(
        from = "2004-03-12", to = "2004-04-11",
        format = "%Y-%m-%d", FinCenter = "GMT")
    print(tS)
    tS = timeSequence(
        from = "2004-03-12", to = "2004-04-11",
        format = "%Y-%m-%d", zone = "Zurich", FinCenter = "Zurich")
    print(tS)
   
    
    # HOMEWORK:
    # GMT/Zurich Daily 16:00:
    tS = timeSequence(
        from = "2004-03-12 16:00:00", to = "2004-04-11 16:00:00",
        format = "%Y-%m-%d %H:%M:%S", FinCenter = "GMT")
    print(tS)
    tS = timeSequence(
        from = "2004-03-12 16:00:00", to = "2004-04-11 16:00:00",
        format = "%Y-%m-%d %H:%M:%S", FinCenter = "Europe/Zurich")
    print(tS)
    tS = timeSequence(
        from = "2004-03-12 16:00:00", to = "2004-04-11 16:00:00",
        format = "%Y-%m-%d %H:%M:%S", zone = "Zurich", FinCenter = "Zurich")
    print(tS)
    tS = timeSequence(
        from = "2003-03-12 16:00:00", to = "2004-02-12 16:00:00",
        by = "month", format = "%Y-%m-%d %H:%M:%S", FinCenter = "GMT")
    print(tS)
    tS = timeSequence(
        from = "2003-03-12 16:00:00", to = "2004-02-12 16:00:00",
        by = "month", format = "%Y-%m-%d %H:%M:%S", FinCenter = "Zurich")    
    print(tS)
    

# ------------------------------------------------------------------------------


### Example 2.3.7: Print and Format Functions


    # HOMEWORK:
    # print - Print 'timeDate' Object:
    print(ZURICH)
 
    
    # HOMEWORK:
    # format.timeDate - Format as ISO Conform String:
    format(ZURICH)  
    

# ------------------------------------------------------------------------------
    

### Example 2.3.8: 'timeDate' Methods


    # "[" -
    # Extracts or Replaces Subsets From 'timeDate' Objects:
    # Zurich 'timeDate':
    print(ZURICH)
    n = length(format(ZURICH))
    n
    # Revert:
    tD = ZURICH[n:1]
    print(tD)
    # Skip the first three:
    tD = ZURICH[-(1:3)]
    print(tD)
    # Get the second and the fourth:
    tD = ZURICH[c(2, 4)]
    print(tD)
  
    
    # "+.timeDate" | "-.timeDate" -
    # Performs + or - Operation on 'timeDate' Objects in seconds:
    # Add/Subtract One Day:
    print(ZURICH)
    tD = ZURICH + 24*3600
    print(ZURICH)
    tD = ZURICH - 24*3600
    print(ZURICH)

    
    # HOMEWORK:
    # Difftime Object:
    n = length(ZURICH)
    n
    tD = ZURICH[2:n] - ZURICH[1:(n-1)]
    print(tD)

    
    # HOMEWORK:
    # Ops.timeDate -
    # Groups 'Ops' Generic Functions for 'timeDate' Vectors:
    # Logical Operations:
    tD = ZURICH[ZURICH < ZURICH[3]]
    print(tD)
    # Next:
    ZURICH[ZURICH < ZURICH[3]] == ZURICH[1:3]

    
    # HOMEWORK:
    # diff.timeDate  -
    # Generates Suitably Lagged and Iterated Differences:
    print(ZURICH)
    # Compute Differences:
    diff(ZURICH)
    # Lag 3:
    diff(ZURICH, lag = 3)
    # Second order differences:
    diff(ZURICH, lag = 1, differences = 2)

    
    # HOMEWORK:
    # difftimeDate -
    # Computes Time Differences:
    DIFF = difftimeDate(ZURICH[1:2], ZURICH[-(1:2)])
    DIFF
    DIFF = difftimeDate(ZURICH[1:2], ZURICH[-(1:2)], units = "secs")
    DIFF

    
    # c.timeDate -
    # Concatenates Objects of Class 'timeDate':
    # Concatenate:
    tD = c(GMT[1:2], ZURICH[1:2])
    print(tD)
    tD = c(ZURICH[1:2], GMT[1:2])
    print(tD)

    
    # HOMEWORK:
    # rep.timeDate -
    # Replicates Objects of Class 'timeDate':
    tD = rep(ZURICH[2], times = 3)
    print(tD)
    tD = rep(ZURICH[2:3], times = 2)
    print(tD)
    
    
    # HOMEWORK:
    # round.timeDate -
    # Rounds Objects of Class 'timeDate':
    print(ZURICH)
    tD = round(ZURICH, units = "hours")
    print(tD)

    
    # HOMEWORK:
    # trunc.timeDate -
    # Truncates Objects of Class 'timeDate':
    print(ZURICH)
    tD = trunc(ZURICH, units = "hours")
    print(tD)

    
    # start | end -
    # Extracts the first/last Object of a 'timeDate':
    print(ZURICH)
    print(start(ZURICH))
    print(end(ZURICH))

    
    # HOMEWORK:
    # sort.timeDate -
    # Sorts the Objects of a 'timeDate' Vector:
    print(ZURICH)
    print(sort(ZURICH)) 
    print(sample(ZURICH))


# ------------------------------------------------------------------------------


### Example 2.3.9: More 'timeDate' Methods 
    

    # HOMEWORK:
    # as.character | as.data.frame | as.POSIXct | as.POSIXlt -
    # Convert "timeDate" Objects:
    # Character:
    as.character(ZURICH)
    # Data Frame:
    as.data.frame(ZURICH)
    # POSIX:
    as.POSIXct(ZURICH)

    
    # HOMEWORK:
    # julian.timeDate -
    # Extracts Julian Time in Days Since 1970-01-01:
    # Transform to Julian:
    julian(ZURICH)
    as.integer(julian(ZURICH))
    # Day Units:
    julian(ZURICH, units = "days")
    as.integer(julian(ZURICH, units = "days"))

    
    # atoms.timeDate -
    # Returns "Atoms" From a 'timeDate' Object:
    # Extract Calendar Atoms:
    print(ZURICH)
    atoms(ZURICH)
    # Days:
    atoms(ZURICH)[, 3]
    # Columnames are: "Y", "m", "d", "H", "M", "S"
    atoms(ZURICH)[, "d"]   
    
   
################################################################################
# Chapter 2.4 - Rmetrics 'timeSeries' Objects


### Example 2.4.1: Load Johnson-Johnson Data


    # Use JohnsonJohnson from 'stats'
    # Load data - it's a data frame:
    data(JohnsonJohnson)
    JohnsonJohnson
    class(JohnsonJohnson)
    
    
# ------------------------------------------------------------------------------

    
### Example 2.4.2: Convert Johnson-Johnson into 'timeSeries'

    
    # Create a 'timeSeries' object
    args(timeSeries)
    myFinCenter = "GMT"
    tD = timeCalendar(
        y = rep(1960:1980, each = 4),
        m = rep(c(3,6,9,12), 21), 
        d = rep(c(31,30,30,31), 21))
    Data = as.vector(JohnsonJohnson)
    JJ = timeSeries(data = Data, charvec = tD, units = "JJ")
    head(JJ)
    
    
    # HOMEWORK:
    # What Class?
    class(JJ)
    # [1] "timeSeries"
    # attr(,"package")
    # [1] "fCalendar"
    is.timeSeries(JJ)
    # [1] TRUE
   
    
# ------------------------------------------------------------------------------
 
    
### Example 2.4.3: Show Johnson-Johnson Slots


    # HOMEWORK:
    # Slot Names ?
    slotNames(JJ)
    
    
    # HOMEWORK:
    # Data Slot:
    jj.mat = JJ@Data
    # What Class?
    class(jj.mat)
    # [1] "matrix"
    # Show the Head of the Matrix:
    head(jj.mat)

    
    # HOMEWORK:
    # Position Slot:
    positions = JJ@positions
    head(positions)
    # What Class?
    class(positions)
    
      
# ------------------------------------------------------------------------------

   
### Example 2.4.4: Compute Johnson-Johnson Returns
    
    
    # Arguments:
    args(returnSeries)
    
    
    # Compute Monthly Returns:
    JJ.RET = returnSeries(JJ, units = "JJ.RET")
    # Print the Head of the Series
    head(JJ.RET)  
    

# ------------------------------------------------------------------------------

    
### Example 2.4.5: Aggregate Johnson-Johnson 'timeSeries'
    
    
    # Arguments:
    args(applySeries)
  
    
    # applySeries - Aggregate the 'Return' Series Annually:
    print(c(start(JJ.RET), end(JJ.RET)))
    
    
    # Define Blocks - Unfortunately there is not yet a by="annual":
    from = timeCalendar(y = 1960:1980, m = rep(1, 21), d = rep(1, 21))
    to = timeCalendar(y = 1960:1980, m = 12, d = 31)
    # Print Annual Blocks:
    print(from[1:3])
    print(to[1:3])
    
    
    # Aggregate the Series:
    JJANNUAL.RET = applySeries(JJ.RET, from, to, FUN = sum)
    # Update Column Names:
    colnames(JJANNUAL.RET)<-"AnnChanges"
    # Print the Head of the Series:
    head(JJANNUAL.RET)
    

# ------------------------------------------------------------------------------

    
### Example 2.4.6: Merge Johnson-Johnson 'timeSeries'
    
    
    # Merge the 'timeSeries' with the end of year earnings
    args(merge.timeSeries)

    
    # Merge:
    # My Remark - It would be nice to have apply as a generic function
    JJ.LAST = applySeries(JJ, from, to, FUN = function(x) rev(x)[1])
    colnames(JJ.LAST)<-"Earnings"
    JJ2 = merge(JJANNUAL.RET, JJ.LAST)
    head(JJ2)

    
    # HOMEWORK:
    # Print Start and end Date:
    print(c(start(JJ2), end(JJ2)))
    

# ------------------------------------------------------------------------------

    
### Example 2.4.7: Cut a Piece from the Johnson-Johnson 'timeSeries'


    # Cut Out a Piece from a 'timeSeries' Object:
    args(cut.timeSeries)

    
    # Cut the Last 5 years:
    JJ.CUT = cut(JJ2, from = "1976-03-31", to = "1980-12-31")
    head(JJ.CUT)
    
    
    # HOMEWORK:
    # Or:
    JJ.CUT = JJ2[JJ2@positions > "1976-03-31"]
    print(JJ.CUT)
    # Or:
    JJ.CUT = JJ2[seriesPositions(JJ2) > timeDate("1976-03-31")]
    print(JJ.CUT)
    
    
# ------------------------------------------------------------------------------    
    
    
### Example 2.4.8: Microsoft Data

    
    # Load Data:
    URL = "http://localhost/econophysics/R/data/textbooks/ZivotWang/data/msft.dat.csv"
    download.file(URL, "MSFT.CSV")
    MSFT = readSeries("MSFT.CSV")
     
    
    # HOMEWORK:
    # Print first 5 Lines:
    head(MSFT, 5)
    # Print Range: Start - End:
    print(c(start(MSFT), end(MSFT)))

    
    # Cut out April Data from 2001 with last March Days:
    april.ts = cut(MSFT, "2001-03-30", "2001-04-30")
    print(april.ts)
    
    
# ------------------------------------------------------------------------------
  
          
### Example 2.4.9: Microsoft April Cloasings


    # Merge Closing Prices and Returns:
    aprilClose.ts = (april.ts[, "Close"])
    aprilReturns.ts = returnSeries(aprilClose.ts, percentage = TRUE, units = "Return")
    aprilCloseReturns.ts = merge(aprilClose.ts[-1], aprilReturns.ts)
    head(aprilCloseReturns.ts)
    
 
# ------------------------------------------------------------------------------

   
### Example 2.4.10: Microsoft Data Alignment


    # Note, April 13th, was Good Friday!
    print(GoodFriday(2001))

    
    # Daily Align:
    args(alignDailySeries)
    # function (x, method = c("before", "after", "interp", "fillNA"), 
    #   include.weekends = FALSE, units = NULL, zone = myFinCenter, 
    #   FinCenter = myFinCenter) 
    # NOTE: Remeplace myFinCenter by x@FinCenter in next Version
    
    
    # Align to Full Calendar Dates, excluding weekends,
    # ... Interpolate Good Friday:
    # NOTE: Remove printing for time difference in next Version
    aprilInterp.ts = alignDailySeries(aprilCloseReturns.ts, 
        method = "interp", units = c("CloseItp", "ReturnItp"))   
    print(aprilInterp.ts)
    
    
# ------------------------------------------------------------------------------

   
### Example 2.4.11: From New York to Zurich Series

    
    # Assume the prices were recorded 16:00 local time New York:
    Dates = format(aprilCloseReturns.ts@positions)
    Positions = paste(Dates, "16:00:00")
    print(Positions)

    
    # New York time series:
    NY.ts = timeSeries(data = aprilCloseReturns.ts@Data, 
        charvec = Positions, units = aprilCloseReturns.ts@units, 
        zone = "NewYork", FinCenter = "NewYork")
    # Print Series:
    print(NY.ts, 4)

    
    # Now use the data which were recorded in NY in Zurich:
    ZH.ts = timeSeries(data = aprilCloseReturns.ts@Data, 
        charvec = Positions, units = aprilCloseReturns.ts@units, 
        zone = "NewYork", FinCenter = "Zurich")
    # Print Series:
    print(ZH.ts, 4)
   
   
# ------------------------------------------------------------------------------

  
### Example 2.4.11: Microsoft Weekly Aggregation


    # Aggregate Weekly:
    # Weekly Mean Price and Total Return:
    to = timeSequence(from = "2001-04-06", length.out = 4, by = "week")
    getDayOfWeek(to)
    from = to - 4*24*3600
    getDayOfWeek(from)

    
    # Sum Column by Column:
    weekly.ts = applySeries(aprilInterp.ts, from, to, 
        FUN = colSums, units = c("meanClose", "totalReturn"))
    print(weekly.ts)    
    # Correct for the Close Mean:
    weekly.ts@Data[, 1] = weekly.ts@Data[, 1 ]/5
    # Print:
    print(weekly.ts)  
   
   
################################################################################
# Chapter 2.5 - Calendarical Calculations


### Exercise: Easter Date Algorithm


    # HOMEWORK:
    # Current Year:
    year = currentYear
    print(year)
    
    
    # HOMEWORK:
    # Calculating the ISO-8601 Date for Easter:
    C = year%/%100
    N = year - 19*(year%/%19)
    K = (C-17)%/%25
    I = C - C%/%4 - (C-K)%/%3 + 19*N + 15
    I = I - 30*(I%/%30)
    I = I - (I%/%28)*(1-(I%/%28)*(29%/%(I+1))*((21-N)/11))
    J = year + year%/%4 + I + 2 - C + C%/%4
    J = J - 7*(J%/%7)
    L = I - J
    month = 3 + (L+40)%/%44
    day = L + 28 - 31*(month%/%4)
    EASTER = as.character(year*10000 + month*100 + day)
    
    
    # HOMEWORK:
    # Print:
    print(EASTER)
    

# ------------------------------------------------------------------------------


### Example: The Date of Easter


    # HOMEWORK:
    print(Easter(2005))
    print(Easter(2004:2011))
    
    
# ------------------------------------------------------------------------------
# Business and Holidays:


    # April 2001:
    tD = timeSequence(from = "2001-04-01", to = "2001-04-30", by = "day",
        zone = "Vienna", FinCenter = "Vienna")
    print(tD)
    
    
    # Extract Weekdays - These are Monday to Friday:
    weekdays = tD[isWeekday(tD)]
    print(weekdays)
    
    
    # Extract Weekends:
    weekends = tD[isWeekend(tD)]
    print(weekends)
    
    
    # Vienna Holiday Calendar:
    holidaysVIENNA = c("NewYearsDay", "GoodFriday", "EasterMonday", "ChristmasDay")
    vienna2001 = holiday(2001, holidaysVIENNA)
    print(vienna2001)
    
    
    # Extract Bizdays:
    bizdays = tD[isBizday(tD, holidays = vienna2001)]
    print(bizdays)
    
    
    # Extract Bizdays:
    holidays = tD[isHoliday(tD, holidays = vienna2001)]
    print(holidays)
    
    
    # Excluding Saturdays and Sundays:
    realHolidays = holidays[isWeekday(holidays)]
    print(realHolidays)

    
# ------------------------------------------------------------------------------
# Day of Week and Day of Year:


    # HOMEWORK:
    # Returns the day of the week to a 'timeDate' object
    getDayOfWeek(Sys.timeDate())    
          
    
    # HOMEWORK:
    # Returns the day of the year to a 'timeDate' object
    getDayOfYear(Sys.timeDate())           
    
    
# ------------------------------------------------------------------------------
# SPECIAL TIMEDATE OPERATIONS:


    #  timeLastDayInMonth     Computes the last day in a given month and year
    #  timeFirstDayInMonth    Computes the first day in a given month and year
    #  timeLastDayInQuarter   Computes the last day in a given quarter and year
    #  timeFirstDayInQuarter  Computes the first day in a given quarter and year
    #  timeNdayOnOrAfter      Computes date in month that is a n-day ON OR AFTER  
    #  timeNdayOnOrBefore     Computes date in month that is a n-day ON OR BEFORE  
    #  timeNthNdayInMonth     Computes n-th ocurrance of a n-day in year/month
    #  timeLastNdayInMonth    Computes the last n-day in year/month

    
    # HOMEWORK:
    # date:
    DATE = c("2006-05-15", "2006-09-12")
    
    
    # HOMEWORK:
    # Computes the last/first day in a given month and year
    print(timeLastDayInMonth(DATE))
    print(timeFirstDayInMonth(DATE))
    
    
    # HOMEWORK:
    # Computes the last/first day in a given quarter and year
    print(timeLastDayInQuarter(DATE))
    print(timeFirstDayInQuarter(DATE))
    
    
    # HOMEWORK:
    # Friday on-or-after:
    getDayOfWeek(timeNdayOnOrAfter(DATE, 5))
    # Monday on or before:
    getDayOfWeek(timeNdayOnOrBefore(DATE, 1))
    
    
    # HOMEWORK:
    # Second Wednesday in May and September:
    getDayOfWeek(timeNthNdayInMonth(DATE, nday = 3, nth = 2))

    
# ------------------------------------------------------------------------------    
        
    
### Example: Time Zones

    
    # HOMEWORK:
    # Note we need "GMT" as time zone
    # Test:
    Sys.putenv(TZ = "GMT")
    Sys.time()
    ###
    
    
# ------------------------------------------------------------------------------


### Example: Calculate Holiday Dates

    
    # HOMEWORK:
    print(holiday(2000:2009, "USLaborDay"))
    print(USLaborDay(2000:2009))
    
   
# ------------------------------------------------------------------------------


### Example: Convert to/from Local Time:

    
    # HOMEWORK:
    # Start with character string:
    charvec = "2004-08-01 00:00:00"
    
    
    # HOMEWORK:
    # Greenwich Mean Time:
    GMT = timeDate(charvec, zone = "GMT", FinCenter = "GMT")
    print(GMT)
    
    
    # HOMEWORK:
    # From GMT to local Zurich time:
    ZUR = timeDate(GMT, zone = "GMT", FinCenter = "Zurich")
    print(ZUR)
    
    
    # HOMEWORK:
    # From Zurich local time to New-York local time:
    NYC = timeDate(ZUR, zone = "Zurich", FinCenter = "NewYork")
    print(NYC)
    
    
    # HOMEWORK:
    # Or, directly from GMT to New-York local time:
    NYC = timeDate(GMT, zone = "GMT", FinCenter = "NewYork")
    print(NYC)
    ###

 
# ------------------------------------------------------------------------------


### Example: Convert Within the Same Time Zone

    
    # What time was it in Berlin at April 5th and 6th, 1980, 
    # at 4:00 PM Zurich time?
    td = c("1980-04-05 16:00:00", "1980-04-06 16:00:00")
    BERLIN = timeDate(td, zone = "Zurich", FinCenter = "Berlin")
    ZURICH = timeDate(td, zone = "Zurich", FinCenter = "Zurich")
    print(BERLIN)
    print(ZURICH)
    # Note, in 1980 Switzerland had no Daylight Savings Time in 
    # contrast, to Germany!


################################################################################

