#####################################################################   
## Farlie-Gumbel-Morgenstern family                                ##
#####################################################################

## Anmerkung: Wenn Daten verwendet werden (zB Renditen des DJI und
## DAX), dann muss zunächst die Randverteilung geschätzt werden. Dann
## gilt zB: U = F(DJI) und V = G(DAX) wobei F und G fuer die je-
## weilige Verteilungsfunktion steht.


.checkValuesFGM <- function(u, v, theta){
    if(any(u < 0) || any(u > 1)) {
    stop("u must be between 0 and 1")
  }
  if(any(v < 0) || any(v > 1)) {
    stop("v must be between 0 and 1")
  }
  if(any(theta < -1) || any(theta > 1)) {
    stop("theta must be between -1 and 1")
  }
}

pfgmCopula <- function(u = 0.5, v = u, theta = 0){
  ## Check Values:
  .checkValuesFGM(u, v, theta)
  ## Calcualte probability
  pC.uv <- u * v + theta * u * v * (1 - u) * (1 - v)
  pC.uv
}

dfgmCopula <- function(u = 0.5, v = u, theta = 0){
  ## Check Values:
  .checkValuesFGM(u, v, theta)
  ## Calculate density:
  dC.uv <- 1 + (-1 + 2 * u) * (-1 + 2 * v) * theta
  dC.uv
}

taufgmCopula <- function(theta){
  if(theta < -1 || theta > 1) {
    stop("invalid theta!")
  }
  else {
    tau <- 2 * theta / 9
    tau
  }
}

rhofgmCopula <- function(theta){
  if(theta < -1 || theta > 1) {
    stop("invalid theta!")
  }
  else {
    rho <- theta/3
    rho
  }
}

  


par(mfrow = c(2,2))

## Plot der Verteilung:

theta <- 0.2
x <- seq(0,1, by = 0.01)
y <- seq(0,1, by = 0.01)
Z <- matrix(NA, nrow = length(x), ncol = length(y))
for(i in 1:length(x)){
  Z[i,] <- pfgmCopula(x[i], y, theta)
}
persp(x, y, Z, main = "FGM Probability", xlab = "u", ylab = "v", zlab =
      "C(u,v)")
contour(x,y,Z, main = "FGM Probabiltiy Contours", xlab = "u", ylab = "v")

## Plot der Dichte:

W <- matrix(NA, nrow = length(x), ncol = length(y))
for(i in 1:length(x)){
  W[i,] <- dfgmCopula(x[i], y, theta)
}
persp(x, y, W, main = "FGM Density", xlab = "u", ylab = "v", zlab =
      "Density C(u,v)")
contour(x, y, W ,main = "FGM Density Contours", xlab = "u", ylab = "v")




#####################################################################   
## Copulas cubic in u and in v                                     ##
#####################################################################

.checkCubic <- function(x, y) {
  if(x^2 - x * y + y^2 - 3 * x + 3 *y <= 0) TRUE
  else FALSE
}

.checkSet <- function(x, y){
  alpha <- x
  beta <- y
  if(alpha * -2 <= beta && beta <= alpha * 1) {
    if(-beta <= alpha && alpha <= 2 * beta) TRUE
    else FALSE
  }
  else FALSE
}

pcubicCopula <- function(u, v, alpha, beta, gamma, delta) {
  if(.checkSet(alpha, beta) || .checkCubic(alpha, beta)) TRUE
  if(.checkSet(alpha, gamma) || .checkCubic(alpha, gamma)) TRUE
  if(.checkSet(delta, beta) || .checkCubic(delta, beta)) TRUE
  if(.checkSet(delta, gamma) || .checkCubic(delta, gamma)) TRUE
  else stop("invalid parameter!!")
 
  pC.uv <- u * v + u * v * (1-u) * (1-v) * (alpha * u * v + beta * u *
  (1-v) + gamma * v* (1-u) + delta * (1-u) * (1-v))
  pC.uv
}

dcubicCopula <- function(u, v, alpha, beta, gamma, delta) {
  if(.checkSet(alpha, beta) || .checkCubic(alpha, beta)) TRUE
  if(.checkSet(alpha, gamma) || .checkCubic(alpha, gamma)) TRUE
  if(.checkSet(delta, beta) || .checkCubic(delta, beta)) TRUE
  if(.checkSet(delta, gamma) || .checkCubic(delta, gamma)) TRUE
  else stop("invalid parameter!!")
      
  dC.uv <- (1 + delta - 2 * u *(-beta +v * ( ( -2 + 3 * v) * alpha + 4
  * (beta + gamma - 2*delta) -3 * v *(beta + 2 *gamma - 2*delta)) + 2
  * delta) + v *((2 - 3*v) * gamma + (-4 + 3 * v) * delta) + 3 * u^2
  * (-beta + delta + 3*v^2*(alpha - beta -gamma + delta) - 2* v* (alpha
  - 2*beta - gamma + 2 *delta)))
  dC.uv
}

taucubicCopula <- function(alpha, beta, gamma, delta){
  if(.checkSet(alpha, beta) || .checkCubic(alpha, beta)) TRUE
  if(.checkSet(alpha, gamma) || .checkCubic(alpha, gamma)) TRUE
  if(.checkSet(delta, beta) || .checkCubic(delta, beta)) TRUE
  if(.checkSet(delta, gamma) || .checkCubic(delta, gamma)) TRUE
  else stop("invalid parameter!!")
  tau <- 1/450 * (-beta * (-25 + gamma) + alpha * (25 + delta) + 25 *
  (gamma + delta) )
  tau
}

rhocubicCopula <- function(alpha, beta, gamma, delta){
  if(.checkSet(alpha, beta) || .checkCubic(alpha, beta)) TRUE
  if(.checkSet(alpha, gamma) || .checkCubic(alpha, gamma)) TRUE
  if(.checkSet(delta, beta) || .checkCubic(delta, beta)) TRUE
  if(.checkSet(delta, gamma) || .checkCubic(delta, gamma)) TRUE
  else stop("invalid parameter!!")
  rho <- 1/12 * (alpha + beta + gamma + delta)
  rho
}   

par(mfrow = c(2,2))

## Plot der Verteilung:

alpha <- 0.2
beta <- 0.3
gamma <- 0.1
delta <- 0.4

x <- seq(0,1, by = 0.01)
y <- seq(0,1, by = 0.01)
Z <- matrix(NA, nrow = length(x), ncol = length(y))
for(i in 1:length(x)){
  Z[i,] <- pcubicCopula(x[i], y, alpha, beta, gamma, delta)
}
persp(x, y, Z, main = "Cubic Probability", xlab = "u", ylab = "v", zlab = "C(u,v)")
contour(x,y,Z, main = "Cubic Probability Contours", xlab = "u", ylab = "v")

## Plot der Dichte:

W <- matrix(NA, nrow = length(x), ncol = length(y))
for(i in 1:length(x)){
  W[i,] <- dfgmCopula(x[i], y, theta)
}
persp(x, y, W, main = "Cubic Density", xlab = "u", ylab = "v", zlab = "C(u,v)")
contour(x, y, W, main = "Cubic Density Contours", xlab = "u", ylab = "v")


#####################################################################   
## Cuadras-Augé Copulas                                            ##
#####################################################################

.checkValuesCuadras <- function(u, v, alpha, beta){
    if(any(u < 0) || any(u > 1)) {
    stop("u must be between 0 and 1")
  }
  if(any(v < 0) || any(v > 1)) {
    stop("v must be between 0 and 1")
  }
  if(any(alpha < 0) || any(alpha > 1)) {
    stop("alpha must be between -1 and 1")
  }
  if(any(beta < 0) || any(beta > 1)) {
    stop("beta must be between -1 and 1")
  }
}

pCuadrasAugeCopula <- function(u, v, alpha, beta) {
  .checkValuesCuadras(u, v, alpha, beta)

  pC.uv <- min(u^(1- alpha) * v, u * v^(1- beta))
  pC.uv
}

dCuadrasAugeCopula <- function(u, v, alpha, beta){
  .checkValuesCuadras(u, v, alpha, beta)
  # browser()
  I <- ((u^(1-alpha) * v) <  (u * v^(1- beta)))
  dC.uv <- ((-u ^(-alpha) * (alpha - 1) * I) + (-v^(-beta)* (beta -1) *
  (1 - I)))
  dC.uv
}

tauCuadrasAugeCopula <- function(alpha, beta) {
  if(any(alpha < 0) || any(alpha > 1)) {
    stop("alpha must be between -1 and 1")
  }
  if(any(beta < 0) || any(beta > 1)) {
    stop("beta must be between -1 and 1")
  }
  else {tau <- alpha * beta / ( alpha - alpha * beta + beta)
        tau
      }
}

rhoCuadrasAugeCopula <- function(alpha, beta) {
  if(any(alpha < 0) || any(alpha > 1)) {
    stop("alpha must be between -1 and 1")
  }
  if(any(beta < 0) || any(beta > 1)) {
    stop("beta must be between -1 and 1")
  }
  else {
    rho <- - (3 * alpha) / (-2 + alpha)
    rho
  }
}


alpha <- 0.3
beta <- 0.7
x <- seq(0,1, by = 0.01)
y <- seq(0,1, by = 0.01)
Z <- matrix(NA, nrow = length(x), ncol = length(y))
for(i in 1:length(x)){
  for(j in 1: length(y)){
    Z[i,j] <- pCuadrasAugeCopula(x[i], y[j], alpha, beta)
  }
}
par(mfrow = c(2,2))
persp(x, y, Z, main = "Cuadras Auge Probability", xlab = "u", ylab =
      "v", zlab = "C(u,v)")
contour(x,y,Z, main = "Cuadras Auge Probability Contours", xlab = "u",
        ylab = "v")

## Plot der Dichte:

x <- seq(0.01, 1, by = 0.01)
y <- seq(0.01, 1, by = 0.01)

cuadrasAugeDensity <- function(x, y, alpha=0.3, beta=0.7){
  dens2d <- matrix(NA, nrow = length(x), ncol = length(y))
  for(i in 1:length(x)){
    for(j in 1:length(y)){
      dens2d[i,j] <- dCuadrasAugeCopula(x[i], y[j], alpha, beta)
    }
  }
  dens2d
}

<<<<<<< .mine
## persp(x, y, W, main = "Cuadras Auge Density", xlab = "u", ylab = "v",
##      zlab = "Density C(u,v)")
## contour(x, y, W, main = "Cuadras Auge Probability Contours", xlab =
##        "u", ylab = "v")
=======
W <- cuadrasAugeDensity(x,y)

persp(x, y, W, main = "Cuadras Auge Density", xlab = "u", ylab = "v",
      zlab = "Density C(u,v)")
contour(x, y, W, main = "Cuadras Auge Probability Contours", xlab =
        "u", ylab = "v")
>>>>>>> .r147

cuadrasAugeSlider = function () 
{
    refresh.code = function(...) {
        N = .sliderMenu(no = 1)
        alpha = .sliderMenu(no = 2)
        beta = .sliderMenu(no = 3)
        theta = .sliderMenu(no = 4)
        phi = .sliderMenu(no = 5)
        
        xmin = 0.01
        xmax = 1
        
        x <- y <- seq(xmin, xmax, length = N)
        dens2d <- cuadrasAugeDensity(x,y,alpha,beta)
        
        main1 = paste("Cuadras Auge Perspective\n", "alpha = ", as.character(alpha), 
            " | ", "beta = ", as.character(beta))
        main2 = paste("Cuadras Auge Contour\n", "xmin 0.01% = ", as.character(xmin), 
            " | ", "xmax 0.99% = ", as.character(xmax), " | ")

        par(mfrow = c(2, 1), cex = 0.7)

        persp(x, y, dens2d, main = main1, xlab = "u", ylab = "v",
              zlab = "Density C(u,v)",phi=phi,theta=theta)

        
        #plot(s, y2, type = "l", xlim = c(xmin, xmax), ylim = c(0, 
        #    1), col = "steelblue")
        #abline(h = 0, lty = 3)
        #abline(h = 1, lty = 3)
        #abline(h = 0.5, lty = 3)
        #  abline(v = mu, lty = 3, col = "red")
        #title(main = main2)
        par(mfrow = c(1, 1), cex = 0.7)
        
    }
    .sliderMenu(refresh.code, names = c("N", "alpha", "beta", "theta", "phi"), minima = c(10, 0, 0, 0, 0), maxima = c(200, 1, 1, 360, 100), resolutions = c(10, 0.1, 0.1, 1, 1), starts = c(50, 0.3, 0.7, 0, 15))
}


## Anwendung:

cuadrasAugeSlider()
