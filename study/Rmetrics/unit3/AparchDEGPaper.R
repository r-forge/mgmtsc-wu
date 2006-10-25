 
 
    # Example from Ding - Granger - Engle
    require(fSeries)

    
# ------------------------------------------------------------------------------
# Data Download:

    
    # Load Data -
    # Note, this may be not exactly the same data set ...
    URL = "http://www.itp.phys.ethz.ch/econophysics/R/data/papers/DingGrangerEngle-Aparch/data/sp500dge.csv"
    download.file(URL, destfile = "sp500dge.csv")
    R = 100 * read.table(file = "sp500dge.csv", header = TRUE)[,1]
    r = R[-1]
    ###
  
    
# ------------------------------------------------------------------------------
# Data Analysis:
 
    
    # Create Table 2.1
    table21 = c(
        "sample size" = length(r),
        mean = mean(r),
        std = sd(r),
        skewness = skewness(r),
        kurtosis = kurtosis(r),
        min = min(r),
        max = max(r),
        "studentized range" = (max(r)-min(r))/sd(r),
        "normality test" =  jarqueberaTest(r)@test$statistic )
    table21
    ###
    
    
    # Compute Abs Return Series:
    X = exp(cumsum(R/100))
    ###
        
    # Plot Figure 2.1 - 2.3
    par(mfrow = c(3,1), cex = 0.7)
    plot(X, type = "l", ylab = "", col = "steelblue", 
        main = "SP500 Daily Price Index")
    grid()
    plot(R, type = "l", ylab = "", col = "steelblue", 
        main = "SP500 Daily Returns")
    grid()
    plot(abs(R), type = "l", ylab = "", col = "steelblue", 
        main = "SP500 Daily Absolute Returns")
    grid()
    ###
    
   
    
    # Create Table 3.1:
    lags = c(1:5, 10, 20, 40, 70, 100)
    ACF = NULL
    ACF = rbind(ACF, acf(r, lag.max = 100, plot = FALSE)$acf[lags+1])
    ACF = rbind(ACF, acf(abs(r), lag.max = 100, plot = FALSE)$acf[lags+1])
    ACF = rbind(ACF, acf(r*r, lag.max = 100, plot = FALSE)$acf[lags+1])
    rownames(ACF) = c("r", "|r|", "r^2")
    colnames(ACF) = c("lag 1", as.character(lags[-1]))
    table31 = round(ACF, digits = 3)
    table31
    ###
        
        
    # Create Table 3.2
    par(mfrow = c(1,1))
    d = c(0.125, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 3)
    ACFD = NULL
    for (D in d) {
        ACFD = rbind(ACFD, 
            acf(abs(r)^D, lag.max = 100, plot = FALSE)$acf[lags+1])
    }
    rownames(ACFD) = as.character(d)
    colnames(ACFD) = c("lag 1", as.character(lags[-1]))
    table32 = round(ACFD, digits = 3)
    table32
    ###
    
    
    # Taylor Plot:
    par(mfrow = c(1, 1))
    teffectPlot(abs(r), deltas = seq(1/8, 3, length = 50), lag.max = 10) 
    ###
    
    
# ------------------------------------------------------------------------------
# NOTE - this is for 2.3 in Rmetrics 2.4.0 - the arguments have been changed ! 

    # Formula (15)
    
    
    # Rmetrics:
    fit = garchFit(~arma(0,1) + garch(1,1), data = r)
    summary(fit)
    ###

    
    # Ox: Estimate MA(1)- GARCH(1,1) With garchOxFit()
    fit = garchOxFit(formu~ arma(0,1) garch(1,1))
    #   
    # Maximum Likelihood Estimation (Std.Errors based on Numerical OPG matrix)
    #                   Coefficient    Std.Error  t-value  t-prob
    # Cst(M)               0.000440  6.0956e-005    7.212  0.0000
    # MA(1)                0.143539    0.0078410    18.31  0.0000
    # Cst(V)               0.007741   0.00062376    12.41  0.0000
    # ARCH(Alpha1)         0.091464    0.0017888    51.13  0.0000
    # GARCH(Beta1)         0.906265    0.0020807    435.6  0.0000
    # 
    # No. Observations :      17053   No. Parameters  :         5
    # Mean (Y)         :    0.00018   Variance (Y)    :   0.00013
    # Skewness (Y)     :   -0.49340   Kurtosis (Y)    :  25.58650
    # Log Likelihood   :  56826.973   Alpha[1]+Beta[1]:   0.99773
    #
    # Warning : To avoid numerical problems, the estimated parameter
    # Cst(V), and its std.Error have been multiplied by 10^4.
    ###
    
    
    # SPlus: Estimate MA(1)- GARCH(1,1) With garch():
    Q = 1
    fit = garch(formula.mean =  ~ma(1), formula.var =  ~garch(1, 1), series = x,
        control = bhhh.control(tol = 0.0001/Q, delta = 0.0001/Q, n.iter = 100*Q))
    #   
    #        C 5.068e-004
    #    MA(1) 1.466e-001
    #        A 8.441e-007
    #  ARCH(1) 9.561e-002
    # GARCH(1) 9.020e-001
    #           
    Q = 1000
    fit = garch(formula.var =  ~garch(1, 1), series = x,
        control = bhhh.control(tol = 0.0001/Q, delta = 0.0001/Q, n.iter = 100*Q))
    #            
    #        C 4.376e-004
    #    MA(1) 1.441e-001
    #        A 7.778e-007
    #  ARCH(1) 9.184e-002
    # GARCH(1) 9.058e-001
      
    
    # Summary MA(1) - GARCH(1,1)
    #
    #           RMETRICS      DGE         SPLUS       SPLUS       OX
    #                                     Q=1         Q=1000
    #       mu  0.000437      0.000438    0.000507    0.000438    0.000440
    #    MA(1)  0.1440        0.144       0.147       0.144       0.1435
    #    omega  0.00775/10^4  0.00000008  0.00000008  0.00000008  0.00774/10^4
    #  ARCH(1)  0.0914        0.091       0.096       0.0918      0.0915
    # GARCH(1)  0.906         0.906       0.902       0.906       0.906
    
    #      LLH                56822                   56827       56827
         
         
# ------------------------------------------------------------------------------
         

    # Formula (19)
    
    # Rmetrics:
    garchFit(~arma(0,1), ~aparch(1,1), symmetric = FALSE)
    
    # Ox: Estimate MA(1)- APGARCH(1,1) With garchOxFit()
    x = as.vector(returnSeries(as.timeSeries(sp500dge)))
    fit = garchOxFit(formula.mean = ~ arma(0,1), formula.var = ~ aparch(1,1))   
    #
    # Maximum Likelihood Estimation (Std.Errors based on Numerical OPG matrix)
    #                   Coefficient   Std.Error  t-value  t-prob
    # Cst(M)               0.000204 6.5105e-005    3.126  0.0018
    # MA(1)                0.144792   0.0076502    18.93  0.0000
    # Cst(V)               0.144021    0.031802    4.529  0.0000
    # ARCH(Alpha1)         0.083615   0.0025485    32.81  0.0000
    # GARCH(Beta1)         0.920108   0.0019257    477.8  0.0000
    # APARCH(Gamma1)       0.376925    0.017956    20.99  0.0000
    # APARCH(Delta)        1.417771    0.042038    33.73  0.0000
    #
    # No. Observations :     17053  No. Parameters  :         7
    # Mean (Y)         :   0.00018  Variance (Y)    :   0.00013
    # Log Likelihood   : 56981.565
    # 
    # Warning : To avoid numerical problems, the estimated parameter
    # Cst(V), and its std.Error have been multiplied by 10^4.
    
    
    # SPlus: Estimate MA(1)- GARCH(1,1) With pgarch():
    garch(formula.mean =  ~ ma(1), formula.var =  ~ pgarch(1, 1), 
        series = x, leverage = T,
        control = bhhh.control(tol = 0.0001/Q, delta = 0.0001/Q, n.iter = 100*Q))
    # Coefficients:                     
    #        C  0.00014862
    #    MA(1)  0.14470844
    #        A  0.00001301
    #  ARCH(1)  0.08155912
    # GARCH(1)  0.92257721
    #    POWER  1.42226585
    #   LEV(1) -0.37864353
    
      
    # Summary MA(1) - APGARCH(1,1)
    #   
    #            DGE        SPLUS       OX
    #       mu   0.00021    0.00015     0.00020
    #    MA(1)   0.145      0.145       0.145
    #    omega   0.000014   0.000013    0.000014
    #  ARCH(1)   0.083      0.082       0.084
    # GARCH(1)   0.920      0.923       0.920
    #    delta   1.43       1.42        1.42
    # gamma(1)  -0.373     -0.379      -0.377
    #      LLH   56974                  56981
        
    
# ------------------------------------------------------------------------------  
  
