library(fMultivar)
library(tseries)


foo <- function(x, t = 1, g = 1) {

LogLikeli <- function(x ,y = x) {

## t darf während der Optimierung nicht kleiner als -pi werden!!!
        if (x[1] <= -pi) 
            return(Inf)
    #t = 3
    #g = 2
    fu = -sum(log(dsgsh(y, x[1], x[2])))                ## t, g)))
    fu
}

r = nlm(f = LogLikeli, p = c(t, g), y = x)
r
}


a <- rsgsh(1000, 2, 0.25)
erg <- foo(a)
t <- erg $estimate[1]
g <- erg $estimate[2]
hist(a, breaks = 100, probability = T)
lines(seq(-5, 20, 0.01), dsgsh(seq(-5, 20, 0.01), t, g), col = 2)
lines(seq(-5, 20, 0.01), dsgsh(seq(-5, 20, 0.01), 2, 0.25), col = 3)


t <- 6
g <- 0.9
a <- rnorm(10000, 0.02, 0.8)

#a <- rsgsh(10000, t, g)
erg <- foo(a)
t_est <- erg $estimate[1]
g_est <- erg $estimate[2]
hist(a, breaks = 100, probability = T)
lines(seq(-5, 20, 0.01), dsgsh(seq(-5, 20, 0.01), t_est, g_est), col = 2)
lines(seq(-5, 20, 0.01), dnorm(seq(-5, 20, 0.01), 0.02, 0.8), col = 3)
# lines(seq(-5, 20, 0.01), dsgsh(seq(-5, 20, 0.01), t, g), col = 3)


######atx Daten

atx <- get.hist.quote("^ATX",quote="Close")
atx=timeSeries(coredata(atx),index(atx))
r_atx <- returnSeries(atx)
hist(as.vector(r_atx),breaks=100,probability=TRUE)
erg <- foo(as.vector(r_atx))
t_est <- erg $estimate[1]
g_est <- erg $estimate[2]
lines(seq(-0.1, 0.1, 0.0001), dsgsh(seq(-0.1, 0.1, 0.0001), t_est, g_est), col = 2)



####### nikkei225

nik <- get.hist.quote("^N225",quote="Close")
nik =timeSeries(coredata(nik),index(nik))
r_nik  <- returnSeries(nik)
r_nik <- na.omit(r_nik)
hist(as.vector(100*r_nik),breaks=100,probability=TRUE)
erg <- foo(as.vector(100*r_nik))
t_est <- erg $estimate[1]
g_est <- erg $estimate[2]
lines(seq(-5, 5, 0.01), dsgsh(seq(-5, 5, 0.01), t_est, g_est), col = 2)




####################


nigFit
function (x, alpha = 1, beta = 0, delta = 1, mu = 0, doplot = TRUE, 
    span = "auto", title = NULL, description = NULL, ...) 
{
    x.orig = x
    x = as.vector(x)
    CALL = match.call()
    steps <<- 0
    enigmle = function(x, y = x) {
        if (x[1] <= 0) 
            return(Inf)
        if (x[3] <= 0) 
            return(Inf)
        if (abs(x[2]) >= x[1]) 
            return(Inf)
        f = -sum(log(dnig(y, x[1], x[2], x[3], x[4])))
        steps <<- steps + 1
        cat("\n Optimization Step:         ", steps)
        cat("\n Objective Function Value:  ", -f)
        cat("\n Parameter Estimates:       ", x[1], x[2], x[3], 
            x[4], "\n")
        f
    }
    r = nlm(f = enigmle, p = c(alpha, beta, delta, mu), y = x, 
        ...)
    if (doplot) {
        if (span == "auto") {
            alpha = r$estimate[1]
            beta = r$estimate[2]
            delta = r$estimate[3]
            mu = r$estimate[4]
            span.min = qnig(0.001, alpha, beta, delta, mu)
            span.max = qnig(0.999, alpha, beta, delta, mu)
            print(span.min)
            print(span.max)
            span = seq(span.min, span.max, length = 100)
        }
        par(err = -1)
        z = density(x, n = 100)
        x = z$x[z$y > 0]
        y = z$y[z$y > 0]
        y.points = dnig(span, alpha = r$estimate[1], beta = r$estimate[2], 
            delta = r$estimate[3], mu = r$estimate[4])
        ylim = log(c(min(y.points), max(y.points)))
        plot(x, log(y), xlim = c(span[1], span[length(span)]), 
            ylim = ylim, type = "p", xlab = "x", ylab = "log f(x)")
        title("NIG: Parameter Estimation")
        lines(x = span, y = log(y.points), col = "steelblue4")
        if (exists("grid")) 
            grid()
    }
    if (is.null(title)) 
        title = "Normal Inverse Gaussian Parameter Estimation"
    if (is.null(description)) 
        description = as.character(date())
    fit = list(estimate = r$estimate, minimum = -r$minimum, code = r$code, 
        gradient = r$gradient, steps = steps)
    new("fDISTFIT", call = as.call(CALL), model = "Normal Inverse Gaussian Distribution", 
        data = as.data.frame(x.orig), fit = fit, title = as.character(title), 
        description = as.character(description))
}


hypFit
function (x, alpha = 1, beta = 0, delta = 1, mu = 0, doplot = TRUE,
    span = "auto", trace = FALSE, title = NULL, description = NULL,
    ...)
{
    x.orig = x
    x = as.vector(x)
    CALL = match.call()
    .trace <<- TRUE
    steps <<- 0
    ehypmle = function(x, y = x) {
        alpha = exp(-x[1])
        beta = alpha * tanh(x[2])
        delta = exp(-x[3])
        mu = x[4]
        lambda = x[5]
        f = -sum(log(dhyp(y, alpha, beta, delta, mu)))
        steps <<- steps + 1
        if (.trace) {
            cat("\n Optimization Step:         ", steps)
            cat("\n Objective Function Value:  ", -f)
            cat("\n Parameter Estimates:       ", alpha, beta,
                delta, mu, "\n")
        }
        f
    }
    r = nlm(f = ehypmle, p = c(-log(alpha), atanh(beta/alpha),
        -log(delta), mu), y = x)
    r$estimate[1] = exp(-r$estimate[1])
    r$estimate[2] = r$estimate[1] * tanh(r$estimate[2])
    r$estimate[3] = exp(-r$estimate[3])
    if (doplot) {
        if (span == "auto") {
            alpha = r$estimate[1]
            beta = r$estimate[2]
            delta = r$estimate[3]
            mu = r$estimate[4]
            span.min = qhyp(0.01, alpha, beta, delta, mu)
            span.max = qhyp(0.99, alpha, beta, delta, mu)
            span = seq(span.min, span.max, length = 100)
        }
        par(err = -1)
        z = density(x, n = 100, ...)
        x = z$x[z$y > 0]
        y = z$y[z$y > 0]
        y.points = dhyp(span, alpha = r$estimate[1], beta = r$estimate[2],
            delta = r$estimate[3], mu = r$estimate[4])
        ylim = log(c(min(y.points), max(y.points)))
        plot(x, log(y), xlim = c(span[1], span[length(span)]),
            ylim = ylim, type = "p", xlab = "x", ylab = "log f(x)",
            ...)
        title("HYP: Parameter Estimation")
        lines(x = span, y = log(y.points), col = "steelblue4")
        if (exists("grid"))
            grid()
    }
    if (is.null(title))
        title = "Hyperbolic Parameter Estimation"
    if (is.null(description))
        description = as.character(date())
    fit = list(estimate = c(alpha = r$estimate[1], beta = r$estimate[2],
        delta = r$estimate[3], mu = r$estimate[4]), minimum = -r$minimum,
        code = r$code, gradient = r$gradient, steps = steps)
    new("fDISTFIT", call = as.call(CALL), model = "Hyperbolic Distribution",
        data = as.data.frame(x.orig), fit = fit, title = as.character(title),
        description = as.character(description))
}
