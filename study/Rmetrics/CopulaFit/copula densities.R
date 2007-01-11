## Dichten von Copulae, die in die Fit-Funktion einbezogen werden.
## Alle Copulae, die zur Verfuegung stehen sollen, muessen hier
## eingefuegt werden.


## FGM (dfgmCopula), Cuadras-Auge (dCuadrasAugeCopula) und Cubic
## (dcubicCopula) Copulas:
## Achtung: nur FGM hat einen Parameter:

source("nelsen-densities.R")

## Archimedische Copulas angelehnt an Funktion von Prof. Wuertz:
## Achtung: Typen 2, 7, 8, 11, 15, 18, 21, 22 werden ueber
## Generatorfunktion erstellt, dh: können nicht so einfach gefittet
## werden.

.darchm2Copula
function (u = 0.5, v = u, alpha = NULL, type = 1:22, output = c("vector", 
    "list")) 
{
    output = match.arg(output)
    type = as.integer(type[1])
    if (is.null(alpha)) {
        alpha = .archmParam(type)$param
    }
    a = alpha
    if (is.list(u)) {
        v = u[[2]]
        u = u[[1]]
    }
    if (is.matrix(u)) {
        v = u[, 1]
        u = u[, 2]
    }
    if (type == 1) {
        c.uv = (1 + a) * u^(-1 - a) * v^(-1 - a) * (-1 + u^(-a) + 
            v^(-a))^(-2 - a^(-1))
    }
    if (type == 2) {
        c.uv = NA
        warning("No 2 alternative not available")
        c.uv = .darchm1Copula(u = u, v = v, alpha = alpha, type = type, 
            output = output)
        return(c.uv)
    }
    if (type == 3) {
        c.uv = (-1 + a^2 * (-1 + u + v - u * v) - a * (-2 + u + 
            v + u * v))/(-1 + a * (-1 + u) * (-1 + v))^3
    }
    if (type == 4) {
        c.uv = exp(-((-log(u))^alpha + (-log(v))^alpha)^(1/alpha)) * 
            (-(-log(u))^alpha * (-log(v))^alpha * ((-log(u))^alpha + 
                (-log(v))^alpha)^(1/alpha) + (-log(u))^alpha * 
                (-log(v))^alpha * ((-log(u))^alpha + (-log(v))^alpha)^(1/alpha) * 
                alpha + (-log(u))^(3 * alpha) * (-log(v))^alpha * 
                ((-log(u))^alpha + (-log(v))^alpha)^(-2 * (alpha - 
                  1)/alpha) + 2 * (-log(u))^(2 * alpha) * (-log(v))^(2 * 
                alpha) * ((-log(u))^alpha + (-log(v))^alpha)^(-2 * 
                (alpha - 1)/alpha) + (-log(u))^alpha * (-log(v))^(3 * 
                alpha) * ((-log(u))^alpha + (-log(v))^alpha)^(-2 * 
                (alpha - 1)/alpha))/log(v)/log(u)/v/u/((-log(u))^(2 * 
            alpha) + 2 * (-log(u))^alpha * (-log(v))^alpha + 
            (-log(v))^(2 * alpha))
    }
    if (type == 5) {
        c.uv = (a * exp(a * (1 + u + v)) * (-1 + exp(a)))/(exp(a) - 
            exp(a + a * u) + exp(a * (u + v)) - exp(a + a * v))^2
    }
    if (type == 6) {
        c.uv = (1 - u)^(-1 + a) * (a - (-1 + (1 - u)^a) * (-1 + 
            (1 - v)^a)) * ((1 - u)^a + (1 - v)^a - (1 - u)^a * 
            (1 - v)^a)^(-2 + a^(-1)) * (1 - v)^(-1 + a)
    }
    if (type == 7) {
        c.uv = NA
        warning("No 7 alternative not available")
        c.uv = .darchm1Copula(u = u, v = v, alpha = alpha, type = type, 
            output = output)
        return(c.uv)
    }
    if (type == 8) {
        c.uv = NA
        warning("No 8 alternative not available")
        c.uv = .darchm1Copula(u = u, v = v, alpha = alpha, type = type, 
            output = output)
        return(c.uv)
    }
    if (type == 9) {
        c.uv = (1 - a - a * log(v) + a * log(u) * (-1 + a * log(v)))/exp(a * 
            log(u) * log(v))
    }
    if (type == 10) {
        c.uv = (2 - v^a + u^a * (-1 + v^a))^(-2 - a^(-1)) * (4 - 
            2 * v^a + u^a * (-2 - (-1 + a) * v^a))
    }
    if (type == 11) {
        c.uv = NA
        warning("No 11 alternative not available")
        c.uv = .darchm1Copula(u = u, v = v, alpha = alpha, type = type, 
            output = output)
        return(c.uv)
    }
    if (type == 12) {
        c.uv = ((-1 + u^(-1))^a * (-1 + a + ((-1 + u^(-1))^a + 
            (-1 + v^(-1))^a)^a^(-1) + a * ((-1 + u^(-1))^a + 
            (-1 + v^(-1))^a)^a^(-1)) * ((-1 + u^(-1))^a + (-1 + 
            v^(-1))^a)^(-2 + a^(-1)) * (-1 + v^(-1))^a)/((-1 + 
            u) * u * (1 + ((-1 + u^(-1))^a + (-1 + v^(-1))^a)^a^(-1))^3 * 
            (-1 + v) * v)
    }
    if (type == 13) {
        c.uv = (exp(1 - (-1 + (1 - log(u))^a + (1 - log(v))^a)^a^(-1)) * 
            (1 - log(u))^(-1 + a) * (-1 + a + (-1 + (1 - log(u))^a + 
            (1 - log(v))^a)^a^(-1)) * (-1 + (1 - log(u))^a + 
            (1 - log(v))^a)^(-2 + a^(-1)) * (1 - log(v))^(-1 + 
            a))/(u * v)
    }
    if (type == 14) {
        c.uv = ((-1 + u^(-a^(-1)))^a * (-1 + v^(-a^(-1)))^a * 
            ((-1 + u^(-a^(-1)))^a + (-1 + v^(-a^(-1)))^a)^(-2 + 
                a^(-1)) * (1 + ((-1 + u^(-a^(-1)))^a + (-1 + 
            v^(-a^(-1)))^a)^a^(-1))^(-2 - a) * (-1 + a + 2 * 
            a * ((-1 + u^(-a^(-1)))^a + (-1 + v^(-a^(-1)))^a)^a^(-1)))/(a * 
            u * (-1 + u^a^(-1)) * v * (-1 + v^a^(-1)))
    }
    if (type == 15) {
        c.uv = NA
        warning("No 15 alternative not available")
        c.uv = .darchm1Copula(u = u, v = v, alpha = alpha, type = type, 
            output = output)
        return(c.uv)
    }
    if (type == 16) {
        c.uv = (2 * a * (a^2 + u^2 * v^2 + a * (u^2 + v^2)))/(sqrt(4 * 
            a + (-1 + u - a * (-1 + u^(-1) + v^(-1)) + v)^2) * 
            (u^2 * v^2 * (-1 + u + v)^2 + a^2 * (u + v - u * 
                v)^2 + 2 * a * u * v * (u^2 * (-1 + v) - (-1 + 
                v) * v + u * (1 - v + v^2))))
    }
    if (type == 17) {
        c.uv = (2^a * ((-1 + 2^a) * a * (1 + u)^a * (1 + v)^a + 
            2^a * (-1 + (1 + u)^a) * (-1 + (1 + v)^a)))/((1 + 
            u) * (1 + v) * (2^a - 2^a * (1 + u)^a - 2^a * (1 + 
            v)^a + (1 + u)^a * (1 + v)^a)^2 * (1 + ((-1 + (1 + 
            u)^(-a)) * (-1 + (1 + v)^(-a)))/(-1 + 2^(-a)))^a^(-1))
    }
    if (type == 18) {
        c.uv = NA
        warning("No 18 alternative not available")
        c.uv = .darchm1Copula(u = u, v = v, alpha = alpha, type = type, 
            output = output)
        return(c.uv)
    }
    if (type == 19) {
        c.uv = (a^3 * exp(a * (u^(-1) + v^(-1))) * (2 + log(-exp(a) + 
            exp(a/u) + exp(a/v))))/((-exp(a) + exp(a/u) + exp(a/v))^2 * 
            u^2 * v^2 * log(-exp(a) + exp(a/u) + exp(a/v))^3)
    }
    if (type == 20) {
        c.uv = (exp(u^(-a) + v^(-a)) * u^(-1 - a) * v^(-1 - a) * 
            log(-exp(1) + exp(u^(-a)) + exp(v^(-a)))^(-2 - a^(-1)) * 
            (1 + a + a * log(-exp(1) + exp(u^(-a)) + exp(v^(-a)))))/(-exp(1) + 
            exp(u^(-a)) + exp(v^(-a)))^2
    }
    if (type == 21) {
        c.uv = NA
        warning("No 21 alternative not available")
        c.uv = .darchm1Copula(u = u, v = v, alpha = alpha, type = type, 
            output = output)
        return(c.uv)
    }
    if (type == 22) {
        c.uv = NA
        warning("No 22 alternative not available")
        c.uv = .darchm1Copula(u = u, v = v, alpha = alpha, type = type, 
            output = output)
        return(c.uv)
    }
    control = list(alpha = alpha[[1]], copula = "archm", type = type)
    attr(c.uv, "control") <- unlist(control)
    if (output == "list") {
        N = sqrt(length(u))
        x = u[1:N]
        y = matrix(v, ncol = N)[1, ]
        c.uv = list(x = x, y = y, z = matrix(c.uv, ncol = N))
    }
    c.uv
}


## Erzeugung derselben Copulas mit Generatorfunktion:

.darchm1Copula
function (u = 0.5, v = u, alpha = NULL, type = 1:22, output = c("vector", 
    "list")) 
{
    output = match.arg(output)
    type = as.integer(type[1])
    if (is.null(alpha)) {
        alpha = .archmParam(type)$param
    }
    if (is.list(u)) {
        v = u[[2]]
        u = u[[1]]
    }
    if (is.matrix(u)) {
        v = u[, 1]
        u = u[, 2]
    }
    c.uv = .invPhiSecondDer(.Phi(u, alpha, type) + .Phi(v, alpha, 
        type), alpha, type)/(.invPhiFirstDer(.Phi(u, alpha, type), 
        alpha, type) * .invPhiFirstDer(.Phi(v, alpha, type), 
        alpha, type))
    control = list(alpha = alpha[[1]], copula = "archm", type = type)
    attr(c.uv, "control") <- unlist(control)
    if (output == "list") {
        N = sqrt(length(u))
        x = u[1:N]
        y = matrix(v, ncol = N)[1, ]
        c.uv = list(x = x, y = y, z = matrix(c.uv, ncol = N))
    }
    c.uv
}
