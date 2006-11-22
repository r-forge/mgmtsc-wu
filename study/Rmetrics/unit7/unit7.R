# Tutorial, 22.11.2006
# theussl

library("fOptions")
library("fMultivar")


GemanYorAsianOption()

gbs <- GBSOption("c", S= 100, X= 100, Time = 1, r=0.1, b=0.1, sigma = 0.3)

ca <- GemanYorAsianOption("c", 100, 100, 1, r=0.1, sigma=0.3)

ca <- GemanYorAsianOption("c", 10, 10, 1, r=0.1, sigma=0.3)


## exakt das gleiche bei BlackScholes
gbs <- GBSOption("c", S= 100, X= 100, Time = 1, r=0.1, b=0.1, sigma = 0.3)
print(gbs)

gbs <- GBSOption("c", S= 100, X= 100, Time = 4, r=0.1/4, b=0.1/4, sigma = 0.3/2)
print(gbs)

# Ungenauigkeit durch Fourier Transformation

system.time(ca <- GemanYorAsianOption("c", 100, 100, 1, r=0.1, sigma=0.3))
system.time(ca <- GemanYorAsianOption("c", 100, 100, 4, r=0.1/4, sigma=0.3/2))



## Monte Carlo Simulation

# Zufallszahlen

args(rnorm.sobol)

par(mfrow=c(2,2))

hist(rnorm.sobol(100,1), breaks="FD", col="steelblue", border = "white")
hist(rnorm.sobol(1000,1), breaks="FD", col="steelblue", border = "white")
hist(rnorm(1000,1), breaks="FD", col="steelblue", border = "white")

R <- rnorm(500)
R <- c(R,-R)
hist(R, breaks="FD", col="steelblue", border = "white")

par(mfrow=c(3,2))

X <- runif.sobol(1000, 2)
head(X)
plot(X, pch =19)
X <- runif.pseudo(1000,2)
plot(X, pch =19)
X <- runif.sobol(1000, 2, scrambling = 0)
plot(X, pch =19)
X <- runif.sobol(1000, 2, scrambling = 1)
plot(X, pch =19)
X <- runif.sobol(1000, 2, scrambling = 2)
plot(X, pch =19)
X <- runif.sobol(1000, 2, scrambling = 3)
plot(X, pch =19)



     ## SOURCE("fOptions.5B-MonteCarloOptions")
        
     ## How to perform a Monte Carlo Simulation?
        
     ## First Step:
        # Write a function to generate the option's innovations. 
        # Use scrambled normal Sobol numbers:
        sobolInnovations = function(mcSteps, pathLength, init, ...) {
          # Create Normal Sobol Innovations:
          innovations = rnorm.sobol(mcSteps, pathLength, init, ...)
          # Return Value:
          innovations }


## --- lv

  round(sobolInnovations(10,6,TRUE),3)
  round(sobolInnovations(10,6,FALSE),3)
  round(sobolInnovations(20,6,TRUE),3)
  round(sobolInnovations(10,6,TRUE, scrambling=1),3)


      pseudoInnovations = function(mcSteps, pathLength, ...) {
          # Create Normal Sobol Innovations:
          innovations = rnorm.pseudo(mcSteps, pathLength)
          # Return Value:
          innovations }

   pseudoInnovations(20, 6)

## ---


     ## Second Step: 
        # Write a function to generate the option's price paths.  
        # Use a Wiener path:
        wienerPath = function(eps) { 
          # Note, the option parameters must be globally defined!
          # Generate the Paths:
          path = (b-sigma*sigma/2)*delta.t + sigma*sqrt(delta.t)*eps
          # Return Value:
          path }

## --- LV

    b <<- 0.1
    delta.t <<- 1/365
    sigma <<- 0.3
    wienerPath(sobolInnovations(40,6, TRUE)[25,])


## ----

     ## Third Step: 
        # Write a function for the option's payoff
        
        # Example 1: use the payoff for a plain Vanilla Call or Put:
        plainVanillaPayoff = function(path) { 
          # Note, the option parameters must be globally defined!
          # Compute the Call/Put Payoff Value:
          ST = S*exp(sum(path))
          if (TypeFlag == "c") payoff = exp(-r*Time)*max(ST-X, 0)
          if (TypeFlag == "p") payoff = exp(-r*Time)*max(0, X-ST)
          # Return Value:
          payoff }

## --- LV

S <<- 100
X <<- 100
Time <<- 1
r <<- 0.1
b <<- 0.1
TypeFlag <<- "c"

## --- 



        # Example 2: use the payoff for an arithmetic Asian Call or Put:
        arithmeticAsianPayoff = function(path) { 
          # Note, the option parameters must be globally defined!
          # Compute the Call/Put Payoff Value:
          SM = mean(S*exp(cumsum(path)))
          if (TypeFlag == "c") payoff = exp(-r*Time)*max(SM-X, 0)
          if (TypeFlag == "p") payoff = exp(-r*Time)*max(0, X-SM)
          # Return Value:
          payoff }

     ## Final Step: 
        # Set Global Parameters for the plain Vanilla / arithmetic Asian Options:
        TypeFlag <<- "c"; S <<- 100; X <<- 100
        Time <<- 1/12; sigma <<- 0.4; r <<- 0.10; b <<- 0.1
        
        # Do the Asian Simulation with scrambled random numbers:
        mc = MonteCarloOption(delta.t = 1/360, pathLength = 30, mcSteps = 5000, 
          mcLoops = 50, init = TRUE, innovations.gen = sobolInnovations, 
          path.gen = wienerPath, payoff.calc = arithmeticAsianPayoff, 
          antithetic = TRUE, standardization = FALSE, trace = TRUE, 
          scrambling = 2, seed = 4711)

## --- LV

        mc = MonteCarloOption(delta.t = 1/360, pathLength = 30, mcSteps = 5000, 
          mcLoops = 50, init = TRUE, innovations.gen = pseudoInnovations, 
          path.gen = wienerPath, payoff.calc = plainVanillaPayoff, 
          antithetic = FALSE, standardization = FALSE, trace = TRUE, 
          scrambling = NA, seed = 4711)


        mc = MonteCarloOption(delta.t = 1/360, pathLength = 30, mcSteps = 10000, 
          mcLoops = 100, init = TRUE, innovations.gen = pseudoInnovations, 
          path.gen = wienerPath, payoff.calc = plainVanillaPayoff, 
          antithetic = FALSE, standardization = FALSE, trace = TRUE, 
          scrambling = NA, seed = 4711)


      ## GBSOption theoretische Wert


        mc = MonteCarloOption(delta.t = 1/720, pathLength = 60, mcSteps = 10000, 
          mcLoops = 100, init = TRUE, innovations.gen = pseudoInnovations, 
          path.gen = wienerPath, payoff.calc = plainVanillaPayoff, 
          antithetic = FALSE, standardization = FALSE, trace = TRUE,         
          scrambling = NA, seed = 4711)

        mc = MonteCarloOption(delta.t = 1/720, pathLength = 60, mcSteps = 10000, 
          mcLoops = 100, init = TRUE, innovations.gen = pseudoInnovations, 
          path.gen = wienerPath, payoff.calc = plainVanillaPayoff, 
          antithetic = TRUE, standardization = FALSE, trace = TRUE, 
          scrambling = NA, seed = 4711)

        mc = MonteCarloOption(delta.t = 1/720, pathLength = 60, mcSteps = 10000, 
          mcLoops = 100, init = TRUE, innovations.gen = sobolInnovations, 
          path.gen = wienerPath, payoff.calc = plainVanillaPayoff, 
          antithetic = TRUE, standardization = FALSE, trace = TRUE, 
          scrambling = 1, seed = 4711)

## epsilon durch eps ersetzen
7
MonteCarloOption <- function (delta.t, pathLength, mcSteps, mcLoops, init = TRUE, 
    innovations.gen, path.gen, payoff.calc, antithetic = TRUE, 
    standardization = FALSE, trace = TRUE, ...) 
{
    delta.t <<- delta.t
    if (trace) 
        cat("\nMonte Carlo Simulation Path:\n\n")
    iteration = rep(0, length = mcLoops)
    cat("\nLoop:\t", "No\t")
    for (i in 1:mcLoops) {
        if (i > 1) 
            init = FALSE
        eps = innovations.gen(mcSteps, pathLength, init = init, 
            ...)
        if (antithetic) 
            eps = rbind(eps, -eps)
        if (standardization) 
            eps = (eps - mean(eps))/sqrt(var(as.vector(eps)))
        path = t(path.gen(eps))
        payoff = NULL
        for (j in 1:dim(path)[1]) payoff = c(payoff, payoff.calc(path[, 
            j]))
        iteration[i] = mean(payoff)
        if (trace) 
            cat("\nLoop:\t", i, "\t:", iteration[i], sum(iteration)/i)
    }
    if (trace) 
        cat("\n")
    iteration
}

plot(mc, type="l")
abline(h=mean(mc), col="3")
abline(h= GBSOption("c", 100,100,1/12,0.1,0.1,0.4)@price, col="2")


        mc = MonteCarloOption(delta.t = 1/1440, pathLength = 120, mcSteps = 10000, 
          mcLoops = 100, init = TRUE, innovations.gen = sobolInnovations, 
          path.gen = wienerPath, payoff.calc = plainVanillaPayoff, 
          antithetic = TRUE, standardization = FALSE, trace = TRUE, 
          scrambling = 3, seed = 4711)


        TypeFlag <<- "c"; S <<- 2; X <<- 2
        Time <<- 1; sigma <<- 0.1; r <<- 0.02; b <<- 0.02



        mc = MonteCarloOption(delta.t = 1/360, pathLength = 360, mcSteps = 1000, 
          mcLoops = 50, init = TRUE, innovations.gen = sobolInnovations, 
          path.gen = wienerPath, payoff.calc = arithmeticAsianPayoff, 
          antithetic = TRUE, standardization = FALSE, trace = TRUE, 
          scrambling = 3, seed = 4711)

       GemanYorAsianOption("c", 2, 2, 1, 0.02, 0.1)


## ---


        # Plot the MC Iteration Path:
        par(mfrow = c(1, 1))
        mcPrice = cumsum(mc)/(1:length(mc))
        plot(mcPrice, type = "l", main = "Arithmetic Asian Option", 
          xlab = "Monte Carlo Loops", ylab = "Option Price")
        
        # Compare with Turnbull-Wakeman Approximation:
        TW = TurnbullWakemanAsianApproxOption(TypeFlag = "c", S = 100, SA = 100, 
          X = 100, Time = 1/12, time = 1/12, tau = 0 , r = 0.1, b = 0.1, 
          sigma = 0.4)$price
        print(TW)
        abline(h = TW, col = 2)



