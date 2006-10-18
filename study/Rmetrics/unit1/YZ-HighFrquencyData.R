
# Bingcheng Yan and Eric Zivot
# 
# Analysis of High-Frequency Financial Data with S-Plus
#
# Introduction
#
# High-frequency financial data are observations on financial variables taken 
# daily or at a finer time scale, and are often irregularly spaced over time. 
# Advances in computer technology and data recording and storage have made 
# these data sets increasingly accessible to researchers and have driven the 
# data frequency to the ultimate limit for some financial markets: time stamped 
# transaction-by-transaction or tick-by-tick data, referred to as ultra-high-
# frequency data by Engle (2000). For equity markets, the Trades and Quotes 
# (TAQ) database of the New York Stock Exchange (NYSE) contains all recorded 
# trades and quotes on NYSE, AMEX, NASDAQ, and the regional exchanges from 1992 
# to present. The Berkeley Options Data Base recorded similar data for options 
# markets from 1976 to 1996. In foreign exchange markets, Olsen Associates in 
# Switzerland maintains a data base of indicative FX spot quotes for many major 
# currency pairs published over the Reuters’ network since the mid 1980’s.


# Load Rmetrics Package:
require(fCalendar)


# Data URL: - or localhost - > www.itp.phys.ethz.ch
URL = "http://localhost/econophysics/R/data/papers/YanZivot-HighFrequency/data/"

# Files:
url.eurusd = paste(URL, "eur_usd.txt", sep = "")
url.get    = paste(URL, "trade_ge.txt", sep = "")
url.geq    = paste(URL, "quote_ge.txt", sep = "")
url.msftt  = paste(URL, "trade_msft.txt", sep = "")
url.msftq  = paste(URL, "quote_msft.txt", sep = "")


################################################################################


#    HFLibrary                Rmetrics:
#    ___________________________________________________________________________
#    
#    TAQLoad()                taqLoad()
#    OlsenLoad()              HOMEWORK
#    
#    reorderTS()              sort()
#    rbindtimeSeries()        rbind()
#    tsBW()                   cut()
#    aggregateSeriesHF()      apply()
#                               
#     .                       intervalBounds()
#     .                       applyHFD()
#    plotByDays()             intervalPlot()
#                 
#    align.withinDay()        alignHFD()     
#      align.withinWeek()          .
#    ExchangeHoursOnly()      cutHFD()        
#      FxBizWeekOnly()             .   
#    diff.withinDay()         diffHFD()          
#      diff.withinWeek()           . 
#    tradeDirec()             directionHFD()
#    DurationInInterv()       durationHFD()
#    getMidQuote()            midquoteHFD()      BID/ASK, e.g. FX ONLY
#    PriceChgInInterv()       returnHFD()  
#    SmoothAcrossIntervs()    smoothHFD()      
#      tableSmoother()             .
#    getSpread()              spreadHFD()        BID/ASK, e.g. FX ONLY
#    Genr.RealVol()           volatilityHFD()  

    
################################################################################
# Chapter 2.2 Data Loading
# taqLoad()


taqLoad = 
function(file, type = c("trade", "quote"), fieldsList = NULL, sep = "|", 
skip = 1)
{  
    # Description:
    #   A Function to load TAQ trade or quote data sets from ASCII file
    #   it outputs a timeSeries object
    
    # Arguments:
    #   file - path of the TAQ data file
    #   type - has to be either "trade", or "quote"
    #   fieldsList - a list object supplying field names and data types   
    #       in the TAQ data file, in the format of the "what" list used 
    #       in scan(). The default is to use the included "trade" or 
    #       "quote" fieldsList.  If the fieldsList is provided, the 
    #       "type" argument is ignored. However, the supplied fieldsList 
    #       must contain the components of "Date" and "Time".
    #   sep - the separator used in the TAQ data file, to be passed to 
    #       scan()
    #   skip - the number of rows to skip when reading the data The data 
    #       set to be loaded must have the date format as "ddmmyyyy" 
    #       and the time format as the number of seconds since the  
    #       midnight of the day.    
    
    # TAQ Data Format:
    # Trade Data:
    #   cond |ex |symbol |corr |g127 |price    |siz  |tdate     |tseq |ttim  |
    #   T    |T  |MSFT   |0    |0    |121.125  |1500 |01MAY1997 |0    |28862 |
    #   T    |T  |MSFT   |0    |0    |121.5625 |500  |01MAY1997 |0    |28944 |
    #   T    |T  |MSFT   |0    |0    |121.5625 |1000 |01MAY1997 |0    |29000 |
    #   T    |T  |MSFT   |0    |0    |121.5625 |1200 |01MAY1997 |0    |29002 |
    #   T    |T  |MSFT   |0    |0    |121.625  |1000 |01MAY1997 |0    |31095 |
    # Quote Data:
    #   ex |mmid |symbol |bid     |bidsiz |mode |ofr     |ofrsiz |qdate      |qseq |qtim  |
    #   T  |     |MSFT   |121.5   |11     |12   |121.625 |11     |01MAY1997  |0    |29844 |
    #   T  |     |MSFT   |121.75  |10     |12   |121.625 |11     |01MAY1997  |0    |32444 |
    #   T  |     |MSFT   |121.75  |10     |12   |121.625 |10     |01MAY1997  |0    |32847 |
    #   T  |     |MSFT   |121.875 |10     |12   |121.625 |10     |01MAY1997  |0    |33390 |
    #   T  |     |MSFT   |121.875 |10     |12   |121.625 |3      |01MAY1997  |0    |33629 |
     
    # Start:
    startTime = Sys.time()
                       
    # Check Field List:
    type = match.arg(type)
    if (is.null(fieldsList)){
        if (type == "trade") {
            fieldsList = list(Cond = "", Ex = "", Symbol = "", Corr = 0,
                G127 = 0, Price = 0, Size = 0, Date = "", Seq = 0, Time = 0)
        } else if (type == "quote") {
            fieldsList = list(Ex = "", MMID = "", Symbol = "", Bid = 0, 
            BidSize = 0, Mode = 0, Ask = 0, AskSize = 0, Date = "", 
            Seq = 0, Time = 0)
        } 
    }
    
    # Check for Time and Date Columns:    
    Names = names(fieldsList)
    stopifnot(any(Names == "Time") & any(Names == "Date"))    
    
    # Separate numeric and character type field.list elements:
    numericList = characterList = fieldsList 
    for (i in 1:length(fieldsList)) {
        if (is.character(fieldsList[[i]])) numericList[i] = NA
        if (is.numeric(fieldsList[[i]])) characterList[i] = NA
    }
    numericList = numericList[!is.na(numericList)]
    characterList = characterList[!is.na(characterList)]
    numericNames = names(unlist(numericList)) 
    characterNames = names(unlist(characterList)) 
    numericNames = numericNames[numericNames != "Time"]
    numericNames = numericNames[numericNames != "Date"]
    characterNames = characterNames[characterNames != "Time"]
    characterNames = characterNames[characterNames != "Date"]
     
    # Scan File and convert to data frame:
    input = scan(file = file, what = fieldsList, sep = sep, 
        skip = skip, multi.line = TRUE, strip.white = TRUE)      
    df = as.data.frame(input)
    
    # Numeric Data Matrix:
    data = as.matrix(df[, numericNames]) 
    colnames(data)<-numericNames
    
    # Character recordIDs Data Frame:
    recordIDs = df[, characterNames] 
    colnames(recordIDs)<-characterNames

    # Time Series Positions:
    format = "%d%B%Y"
    Date = strptime(as.character(df[, "Date"]), format) + df[, "Time"]
    charvec = as.character(Date)
    charvec = timeDate(charvec, zone = "Eastern", FinCenter = "Eastern") 
    
    #  Compose time Series:
    ans = timeSeries(data, charvec, zone = "Eastern", FinCenter = "Eastern",
        units = numericNames, recordIDs = recordIDs)
    
    # Timing:
    execTime = Sys.time() - startTime
    print(execTime)
    
    # Return Value:
    ans
}


# Try - Data Loading:


# Trades:
download.file(url.msftt, "msftt")
msftt.tS = taqLoad(file = "msftt")
head(msftt.tS)
head(msftt.tS@recordIDs)

class(msftt.tS)
slotNames(msftt.tS)
print(seriesPositions(msftt.tS)[1:5])


# Quotes:
download.file(url.msftq, "msftq")
msftq.tS = taqLoad(file = "msftq", type = "quote")
head(msftq.tS)
head(msftq.tS@recordIDs)


# HOMEWORK:
# Write a function 'olsenLoad()' for FX tick-by-tick Data


################################################################################
# Chapter 2.3 - Data Examination and Cleaning


msftt.tS = sort(msftt.tS)



################################################################################
# Some Internal Functions:
# .intervalRange
# .intervalSize
# .uniqueDates


.intervalRange =
function(typeOfBounds = c("daily", "weekly"))
{   
    # Description:
    #   Sets market opening hours defaults for intraday data management
    
    # Details:
    #   The default market opening hours working with daily data will
    #     be in local time from 09:30:00 to 16:00:00 (NYSE), 
    #   for weekly open OTC markets like the FX market this will be usually 
    #     from Sunday night 22:00:00 GMT until Friday night 22:00:00 GMT. 
    
    # Set Default Values for Business Hours:
    typeOfBounds = match.arg(typeOfBounds)
    if (typeOfBounds == "daily") {
        # Default Exchange Openings:
        hours = c("09:30:00", "16:00:00")
    } else if (typeOfBounds == "weekly") {
        # Default OCT Operations from Sunday Night to Friday:
        hours = c("22:00:00", "22:00:00")
    }
    attr(hours, "typeOfBounds")<-typeOfBounds
    
    # Return Value:
    hours
}


# Try:
.intervalRange()


# ------------------------------------------------------------------------------


.intervalSize = 
function(typeOfBounds = c("daily", "weekly"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Sets day-offset default for intraday data management
    
    # Details:
    #   The day-offset 
    #   working on daily exchange data is zero (open and close the same day), 
    #   working on weekly OTC data is 5 days, in seconds 5*24*3600 sec.
    
    # FUNCTION:
    
    # Offset:
    typeOfBounds = match.arg(typeOfBounds)
    if (typeOfBounds == "daily") {
        # Zero offset for Daily Data:
        offset = 0  
    } else if (typeOfBounds == "weekly") {
        # 5 Days (5*24*3600 sec) Offset for Weekly Data:
        offset = 5*24*3600
    }
    
    # Return Value:
    offset
}


# Try:
.intervalSize()


# ------------------------------------------------------------------------------


.uniqueDates =
function(x, typeOfBounds = c("daily", "weekly"), zone = x@FinCenter,
FinCenter = x@FinCenter)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns all unique dates ...
    
    # FUNCTION:
    
    # Check Arguments:
    if (is.timeSeries(x)) x = seriesPositions(x)
    stopifnot(is.timeDate(x))
    typeOfBounds = match.arg(typeOfBounds)
    
    #  Unique Dates:
    from = format(trunc(x[1]))
    to = format(trunc(x[length(x@Data)]))
    ans = timeSequence(from, to,  by = "day", zone = zone, 
        FinCenter = FinCenter) 

    if (typeOfBounds == "weekly") ans = ans[getDayOfWeek(ans) == "Sun"]
    
    # Add Financial Center:
    attr(ans, "FinCenter")<-ans@FinCenter 
    
    # Return Value
    ans
}


# Try:
tD = .uniqueDates(msftt.tS)
print(tD)


################################################################################
# Functions to create Plots on Daily or weekly time intervals:
# intervalBounds()
# applyWithinBounds()
# intervalPlots()


intervalBounds =
function(x, typeOfBounds = c("daily", "weekly"), intervalHours = NULL, 
zone = x@FinCenter, FinCenter = x@FinCenter)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a list of from-to trading interval bounds 
    
    # Details:
    #   On exchanges we have we have daily opening and closing hours
    #   On OTC we have business week restrictions
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(is.timeSeries(x))
    typeOfBounds = match.arg(typeOfBounds)
    
    # Get Offset and Interval Range:
    offset = .intervalSize(typeOfBounds)
    if (is.null(intervalHours)) intervalHours = .intervalRange(typeOfBounds)
     
    # Compute Interval all Interval Ranges:
    X = seriesPositions(x)
    date = .uniqueDates(X, typeOfBounds)
    from = timeDate(paste(as.character(date), intervalHours[1]), 
        zone = zone, FinCenter = FinCenter) 
    to = timeDate(paste(as.character(date+offset), intervalHours[2]), 
        zone = zone, FinCenter = FinCenter) 
     
    # Return Value:   
    list(from = from, to = to)
}


# Try:
intervalBounds(msftt.tS)


# ------------------------------------------------------------------------------


applyWithinBounds =
function(x, typeOfBounds = c("daily", "weekly"), intervalHours = NULL, 
FUN = NULL, ...) 
{
    # Check Arguments:
    stopifnot(is.timeSeries(x))   
    typeOfBounds = match.arg(typeOfBounds)
    if (is.null(intervalHours)) intervalHours = .intervalRange(typeOfBounds)
    stopifnot(nchar(intervalHours) == c(8, 8))
    
    # Get List of Bounds as local timeDate Objects:
    bounds = intervalBounds(x, typeOfBounds, intervalHours) 
    from = bounds$from
    to = bounds$to
    
    # Bounds:
    j.pos = as.POSIXct(seriesPositions(x))
    j.from = as.POSIXct(from)
    j.to = as.POSIXct(to)
    
    # Cut and/or apply:
    if (!is.null(FUN)) fun = match.fun(FUN)
    rowBind = NULL
    for (i in 1:from@Dim) {
        test = (j.pos >= j.from[i] & j.pos <= j.to[i])
        TEST = length(test[test])
        if (TEST > 0) {
            cutted = as.matrix(x@Data[test, ])
            if (is.null(FUN)) {
                ans = cutted
            } else {
                ans = fun(cutted, ...)
            }
            rowBind = rbind(rowBind, ans)
        }
    }
    
    # Create timSeries:
    tS = timeSeries(data = matrix(rowBind), charvec = rownames(rowBind),
        zone = x@FinCenter, FinCenter = x@FinCenter, 
        recordIDs = data.frame(), 
        title = "applyHDF", description = .description() )
     
    # Return Value:   
    tS
}


# Try - Extract Exchange Openings:
tS = applyWithinBounds(msftt.tS[, "Price"])
head(tS)
tail(tS)


# Try - Count Records within Bounds:
colLength = function(x) 
{
    DIM = dim(x)[1]
    ans = matrix(DIM)
    rownames(ans) = rownames(x)[DIM]
    ans
}
tS = applyWithinBounds(msftt.tS[, "Price"], FUN = colLength)
print(tS)
getDayOfWeek(seriesPositions(tS))


# ------------------------------------------------------------------------------
# page 7


intervalPlot =
function(x, typeOfBounds = c("daily", "weekly"), intervalHours = NULL, 
which = NULL, FUN = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description: 
    #   Displays a series of plots for data bounded by trading intervals
    
    # FUNCTION:
    
    # Check:
    stopifnot(isUnivariate(x))
    
    # Internal Plot Function:
    if (is.null(FUN)) {
        fun = function(x, ...) {
            plot(as.POSIXct(rownames(x)), as.vector(x), type = "l", 
                col = "steelblue")
            grid()
            title(main = substr(rownames(x)[1], 1, 10))
            x
        } 
    } else {
        fun = match.fun(FUN)
    }
    
    # Generate Plots:
    ans = applyWithinBounds(x, typeOfBounds, intervalHours, FUN = fun)
    
    # Return Value:
    invisible(ans)
}


# Try:
par(mfrow = c(3, 2), cex = 0.7)
intervalPlot(msftt.tS[, "Price"])


################################################################################
# Page 10 - ExchangeHoursOnly()
# cutHFD()


cutHFD = 
function(x, typeOfBounds = c("daily", "weekly"), intervalHours = NULL) 
{  
    # Tip - FUN = NULL
    
    # Cut:  
    stopifnot(isUnivariate(x))
    ans = applyWithinBounds(x, typeOfBounds, intervalHours, FUN = NULL) 
       
    # Return Value:
    ans
}


tS = cutHFD(msftt.tS[, "Price"])
head(tS)

par(mfrow = c(2, 1), cex = 0.7)
plot(tS, type = "l", col = "steelblue", main = "TAQ Trades")
grid()



# ------------------------------------------------------------------------------
# Page 13 - PriceChgInInterv()
# returnHFD


returnHFD = 
function (x, typeOfBounds = c("daily", "weekly"), intervalHours = NULL) 
{   
    # Log-Difference:
    stopifnot(isUnivariate(x))
    .returns = function(x) { 
        ans = matrix(c(NA, diff(log(x))))
        rownames(ans) = rownames(x)
        ans
    }
    ans = applyWithinBounds(x, typeOfBounds, intervalHours, FUN = .returns) 
    
    # Return Value:
    ans
}


tS = returnHFD(msftt.tS[, "Price"])
head(tS)


# HOMEWORK:
# Add:
# type = c("continuous", "discrete")
# percentage = FALSE, 
# trim = TRUE


# ------------------------------------------------------------------------------
# page 14 - 3.1.2 Duration


durationHFD =
function (x, typeOfBounds = c("daily", "weekly"), intervalHours = NULL) 
{
    # Duration:
    stopifnot(isUnivariate(x))
    # Log-Difference:
    stopifnot(isUnivariate(x))
    .duration = function(x) { 
        ans = matrix(c(NA, as.integer(diff(timeDate(rownames(x))))))
        rownames(ans) = rownames(x)
        ans
    }
    ans = applyWithinBounds(x, typeOfBounds, intervalHours, FUN = .duration) 
    
    # Return Value:
    ans
}

tS = durationHFD(msftt.tS[, "Price"])
head(tS)

# HOMEWORK:
# add trim


# ------------------------------------------------------------------------------
# Page 15 - Spread, FX Only 


# spreadHFD()

# HOMEWORK


# ------------------------------------------------------------------------------
# Page 16 - 3.1.4 Trade Direction

# directionHFD()
# midquoteHFD()


# ------------------------------------------------------------------------------
# Page 17 - 3.1.5 Realized Volatility - NOT SO EASY ...


# volatilityHFD()
# ? timeSpan()


################################################################################
# ADDON:


# smoothHFD()
# alignHFD()   
# aggregateHFD()         


################################################################################
# Statistical Analysis - MORE oR LESS TRIVIAL ...

# ...

################################################################################

