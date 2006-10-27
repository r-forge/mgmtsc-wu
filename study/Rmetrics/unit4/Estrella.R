

# Arturo Estrella and Frederic S. Mishkin
# 
# The Yield Curve as a Predictor of U.S. Recessions
# 
# The yield curve—specifically, the spread between the interest rates on 
# the ten-year Treasury note and the three-month Treasury bill—is a valuable 
# forecasting tool. It is simple to use and significantly outperforms other 
# financial and macroeconomic indicators in predicting recessions two to six 
# quarters ahead.


# ------------------------------------------------------------------------------


#
# Read the paper.
# 
# 1.  Download the data from the Rmetrics Server and have a look on it.
# 
# 2.  Display the data, 
#     - compare the NBER 0|1 "recession" Index with the Stock-Watson 
#       recession probability "xri"
#     - compare the recession Indicator 0|1 with the Stock-Watson recession
#       probability, mapping xr<0.5 onto 0 and xri>0.5 onto 1
#     - Compare the slope of the Yield Curve given by the difference of TBonds
#       and TBills with the NBER Recession Indicator 0|1
# 
# 3.  Fit the Stock-Watson Index Model by TBills and TBonds. Use
#     - a Linear Mode LM, and Robust Linear Model from package MASS
#     - Multivariate Adaptive Spline Models MARS and POLYMARS
#     
# 4.  Try a "Decision Variable" Model and fit 
#     -  a "Probit" and "Logit" Generalized Linear Model.
#     
# 5.  Do an Out-Of-Sample 1 Month Forecast on a Rolling 10 Year Window.
#
     

# ------------------------------------------------------------------------------
# 1. Download the Data:


    # Data URL: www.itp.phys.ethz.ch
    URL = "http://www.itp.phys.ethz.ch/econophysics/R/data/textbooks/Wuertz/data/recession.csv")
    download.file(URL, "recession.csv")
    recession.tS = readSeries("recession.csv")
    
    # Show Head of Data:
    head(recession.tS)
    # Note,
    #   the first column is the NBER recession indicator 0 (no) 1 (recession)
    #   the second column are the 3-month US T-Bills
    #   the third column are the 10-year US T-Bonds
    #   the fourth column is the Stock-Watson recession probability
  
  
# ------------------------------------------------------------------------------  
# 2. Display Data:


    # Print the column Names to see what is in the file ...
    colnames(recession.tS)
    
    # Graph Frame:
    par(mfrow = c(3, 1), cex = 0.7)
    
    # Plot Recession Indicator 0|1 and add Stock-Watson Indicator:
    r = recession.tS[, "recession"]
    xri = recession.tS[, "xri"]
    plot(r, type = "h", lwd = 2, col = "grey", ylab = "Recession")
    title(main = "NBER | Stock-Watson")
    lines(xri)
    abline(h = 0.5, lty = 3, col = "steelblue")
    
    # Plot Recession Indicator 0|1 and add Low|High from Stock-Watson Indicator:
    plot(r, type = "h", lwd = 2, col = "grey", ylab = "Recession")
    title(main = "NBER | Stock-Watson")
    lines((sign(xri-0.5)+1)/2.2, col = "steelblue")
    abline(h = 0.5, lty = 3, col = "steelblue")
    
    # Compare the slope of the Yield Curve with the Recession Indicator:
    bills = recession.tS[, "tbills3m"]
    bonds = recession.tS[, "tbonds10y"]
    plot(r, type = "h", lwd = 2, col = "grey", ylab = "Recession")
    title(main = "NBER | Yield Curve Slope")
    # Scale to the Range [0,1]:
    d = bonds-bills
    d = (d - min(d@Data))/(max(d@Data)-min(d@Data))
    lines(d)
    # What can we learn from this plot?
    
    
#-------------------------------------------------------------------------------
# 3. Fitting in Sample the "xri" by TBills and TBonds 


    # Preprocess the Data ...
   
    # 1/3/6 Month Ahead - Use "xri" Index:
    xlagged.mat = tslag(recession.tS@Data[, "xri"], k = -c(0, 1, 3, 6))
    units = paste("LAG", c(0, 1, 3, 6), sep = "")
    xlagged.tS = timeSeries(xlagged.mat, r@positions, units = units)
    xlagged.tS = merge(xlagged.tS, recession.tS)
    tail(xlagged.tS, 10)
    colnames(xlagged.tS)
    # Interpret this series ...
    
    # Skip the last 6 with NA's
    s = dim(xlagged.tS)[1]
    xlagged.tS = xlagged.tS[-c((s-6):s), ]
    
    
    # Fitting in-Sample a Linear Model:
    
    # Write a Recession Plot Function:
    recessionPlot = function(fit, x, horizon, use) {
        forecast.tS = timeSeries(matrix(fitted(fit)), x@positions)
        plot(x, type = "h", lwd = 3, col = "grey", ylab = "Recession")
        title(main = paste(use, horizon, "Month(s) in-Sample Forecasts"))
        lines(forecast.tS, col = "red")
        abline(h = 0, col = "grey")
        abline(h = 0.5, lty = 3, col = "grey")
    }
    
    # Write a Recession Fit Function:
    recessionFit = function(data, use, FUN, doplot = TRUE, ...) {
        par(mfrow = c(3,1), cex = 0.7)
        funFit = match.fun(FUN)
        # 1 Month In Sample Forecast:
        fit = funFit(LAG1 ~ tbills3m + tbonds10y, data = data, use = use, ...)
        if (doplot) recessionPlot(fit, data[, "xri"], 1, use) 
        # 3 Month In Sample Forecast:
        fit = funFit(LAG3 ~ tbills3m + tbonds10y, data = data, use = use, ...)
        if (doplot) recessionPlot(fit, data[, "xri"], 3, use) 
        # 6 Month In Sample Forecast:
        fit = funFit(LAG6 ~ tbills3m + tbonds10y, data = data, use = use, ...)
        if (doplot) recessionPlot(fit, data[, "xri"], 6, use) 
        invisible()
    }
    
    # Try to Fit with a Linear Model:
    fit = recessionFit(data = xlagged.tS, use = "lm", FUN = "regFit")

    # Try to Fit with a Robust Linear Model:
    fit = recessionFit(data = xlagged.tS, use = "rlm", FUN = "regFit")
    
    # Try to Fit with a Mars and a Polymars Model:
    fit = recessionFit(data = xlagged.tS, use = "mars", FUN = "regFit")
    fit = recessionFit(data = xlagged.tS, use = "polymars", FUN = "regFit")
    

# ------------------------------------------------------------------------------    
# 4. Try a Decision Model - GLM Logit:
# Fitting in-Sample the r by tbills and tbonds - Preprocessing the Data ...

    
    # 1/3/6 Month Ahead - Use "recession" Index:
    rlagged.mat = tslag(recession.tS@Data[, "recession"], k = -c(0, 1, 3, 6))
    units = paste("LAG", c(0, 1, 3, 6), sep = "")
    rlagged.tS = timeSeries(rlagged.mat, r@positions, units = units)
    rlagged.tS = merge(rlagged.tS, recession.tS)
    tail(rlagged.tS, 10)
    colnames(rlagged.tS)
    # skip the last 6 with NA's
    s = dim(rlagged.tS)[1]
    rlagged.tS = rlagged.tS[-c((s-6):s), ]
    
    # Try Generalized Linear Modelling:
    fit = recessionFit(data = rlagged.tS, use = "glm", FUN = "gregFit",
        family = binomial(link = "logit"))
        
    # Try Generalized Linear Modelling:
    fit = recessionFit(data = rlagged.tS, use = "glm", FUN = "gregFit",
        family = binomial(link = "probit"))
  
          
# ------------------------------------------------------------------------------  
# 5. Out-of-sample Forecast Model, this is what we should do ...


    # Concept:
    # Take a Rolling Window over the Sample 
    # let us do it for the 1 month Forecasts using the logit-GLM ...
    
    # Graph Frame:
    par(mfrow = c(2, 1), cex = 0.7)
     
    
    # Prepare the Data:
    dim(rlagged.tS)
    # Create Windows of 10 years length:
    windowSize = 10*12     
    startInvestigation = windowSize + 1
    endInvestigation = dim(rlagged.tS)[1] - 1
    from = rlagged.tS@positions[1:(endInvestigation-windowSize)]
    to =   rlagged.tS@positions[(1+windowSize):endInvestigation]
    head(data.frame(from, to))
    tail(data.frame(from, to))
    N = length(from)
    
    # Redo In-Sample Fit and Plot:
    fit = gregFit(LAG1 ~ tbills3m + tbonds10y, data = rlagged.tS, 
            use = "glm", family = binomial(link = "logit") ) 
    recessionPlot(fit, rlagged.tS[, "recession"], 1, use = "glm") 
    
    # Now Forecast from Each Window:       
    for (i in 1:(N-1)) {
        series.tS = cut(rlagged.tS, from[i], to[i])
        fit = gregFit(LAG1 ~ tbills3m + tbonds10y, data = series.tS, 
            use = "glm", family = binomial(link = "logit") ) 
        next.tS = rlagged.tS[rlagged.tS@positions == to[i+1]]
        forecasted = predict(fit, next.tS)
        forecasted.tS = timeSeries(matrix(forecasted), names(forecasted))
        points(forecasted.tS, pch = 19)
        print(forecasted.tS)
     }
     
     
################################################################################

        
    