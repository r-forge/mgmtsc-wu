foo <- function(x, t = 2, g = 1) {

LogLikeli <- function(x,y=x) {
    #t = 3
    #g = 2
    fu = -sum(log(dsgsh(y, t, g)))
    fu
}

r = nlm(f = LogLikeli, p = c(t, g),y=x)
r
}


####################


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