.archmRange <- 
function (type = 1:22, B = Inf) 
{
    lower = c(-1, 1, -1, 1, -B, 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 
        0, -B, 2, 0, 0, 1, 0)
    upper = c(B, B, 1, B, B, B, 1, B, 1, 1, 0.5, B, B, B, B, 
        B, B, B, B, B, B, 1)
    ans = cbind(lower[type], upper[type])
    rownames(ans) = as.character(type)
    colnames(ans) = c("lower", "upper")
    ans
}

.archmParam <- 
function (type = 1:22) 
{
    type = as.integer(type[1])
    B = Inf
    lower = c(-1, 1, -1, 1, -B, 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 
        0, -B, 2, 0, 0, 1, 0)
    upper = c(B, B, 1, B, B, B, 1, B, 1, 1, 0.5, B, B, B, B, 
        B, B, B, B, B, B, 1)
    Alpha = c(1, 2, 0.5, 2, 1, 2, 0.5, 2, 0.5, 0.5, 0.2, 2, 1, 
        2, 2, 1, 0.5, 3, 1, 1, 2, 0.5)
    ans = list(copula = type)
    ans$param = c(alpha = Alpha[type])
    ans$range = c(lower[type], upper[type])
    ans
}

tFit <- 
function (x, df = 4, doplot = TRUE, span = "auto", trace = FALSE, 
    title = NULL, description = NULL, ...) 
{
    x.orig = x
    x = as.vector(x)
    CALL = match.call()
    steps <<- 0
    etmle = function(x, y = x) {
        if (x[1] <= 0) 
            x[1] = x.save
        f = -sum(log(dt(y, x[1])))
        steps <<- steps + 1
        x.save <<- x[1]
        f
    }
    r = nlm(f = etmle, p = c(df), y = x)
    if (doplot) {
        if (span == "auto") {
            df = r$estimate[1]
            span.min = qt(0.001, df)
            span.max = qt(0.999, df)
            span = seq(span.min, span.max, length = 100)
        }
        par(err = -1)
        z = density(x, n = 100, ...)
        x = z$x[z$y > 0]
        y = z$y[z$y > 0]
        y.points = dt(span, df = r$estimate[1])
        ylim = log(c(min(y.points), max(y.points)))
        plot(x, log(y), xlim = c(span[1], span[length(span)]), 
            ylim = ylim, type = "p", xlab = "x", ylab = "log f(x)", 
            ...)
        title("STUDENT-T: Parameter Estimation")
        lines(x = span, y = log(y.points), col = "steelblue")
        if (exists("grid")) 
            grid()
    }
    if (is.null(title)) 
        title = "Student-t Parameter Estimation"
    if (is.null(description)) 
        description = .description()
    fit = list(estimate = c(df = r$estimate), minimum = -r$minimum, 
        code = r$code, gradient = r$gradient, steps = steps)
    new("fDISTFIT", call = as.call(CALL), model = "Student-t Distribution", 
        data = as.data.frame(x.orig), fit = fit, title = as.character(title), 
        description = .description())
}

ghFit <- 
function (x, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1, 
    doplot = TRUE, span = "auto", trace = FALSE, title = NULL, 
    description = NULL, ...) 
{
    x.orig = x
    x = as.vector(x)
    CALL = match.call()
    .trace <<- trace
    steps <<- 0
    eghmle = function(x, y = x) {
        alpha = exp(-x[1])
        beta = alpha * tanh(x[2])
        delta = exp(-x[3])
        mu = x[4]
        lambda = x[5]
        if (alpha <= 0) 
            return(Inf)
        if (delta <= 0) 
            return(Inf)
        if (abs(beta) >= alpha) 
            return(Inf)
        f = -sum(log(dgh(y, alpha, beta, delta, mu, lambda)))
        f
    }
    r = nlm(f = eghmle, p = c(-log(alpha), atanh(beta/alpha), 
        -log(delta), mu, lambda), y = x)
    r$estimate[1] = exp(-r$estimate[1])
    r$estimate[2] = r$estimate[1] * tanh(r$estimate[2])
    r$estimate[3] = exp(-r$estimate[3])
    if (doplot) {
        if (span == "auto") {
            alpha = r$estimate[1]
            beta = r$estimate[2]
            delta = r$estimate[3]
            mu = r$estimate[4]
            lambda = r$estimate[5]
            span.min = qgh(0.001, alpha, beta, delta, mu, lambda)
            span.max = qgh(0.999, alpha, beta, delta, mu, lambda)
            span = seq(span.min, span.max, length = 100)
        }
        par(err = -1)
        z = density(x, n = 100, ...)
        x = z$x[z$y > 0]
        y = z$y[z$y > 0]
        y.points = dgh(span, alpha = r$estimate[1], beta = r$estimate[2], 
            delta = r$estimate[3], mu = r$estimate[4], lambda = r$estimate[5])
        ylim = log(c(min(y.points), max(y.points)))
        plot(x, log(y), xlim = c(span[1], span[length(span)]), 
            ylim = ylim, type = "p", xlab = "x", ylab = "log f(x)", 
            ...)
        title("HYP: Parameter Estimation")
        lines(x = span, y = log(y.points), col = "steelblue")
        if (exists("grid")) 
            grid()
    }
    if (is.null(title)) 
        title = "Generalized Hyperbolic Parameter Estimation"
    if (is.null(description)) 
        description = .description()
    fit = list(estimate = c(alpha = r$estimate[1], beta = r$estimate[2], 
        delta = r$estimate[3], mu = r$estimate[4], lambda = r$estimate[5]), 
        minimum = -r$minimum, code = r$code, gradient = r$gradient, 
        steps = steps)
    new("fDISTFIT", call = as.call(CALL), model = "Generalized Hyperbolic Distribution", 
        data = as.data.frame(x.orig), fit = fit, title = as.character(title), 
        description = .description())
}

darchmCopula <- 
function (u = 0.5, v = u, alpha = NULL, type = 1:22, output = c("vector", 
    "list"), alternative = FALSE) 
{
    if (alternative) {
        ans = .darchm2Copula(u, v, alpha, type, output)
    }
    else {
        ans = .darchm1Copula(u, v, alpha, type, output)
    }
    ans
}

hypFit <- 
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
        lines(x = span, y = log(y.points), col = "steelblue")
        if (exists("grid")) 
            grid()
    }
    if (is.null(title)) 
        title = "Hyperbolic Parameter Estimation"
    if (is.null(description)) 
        description = .description()
    fit = list(estimate = c(alpha = r$estimate[1], beta = r$estimate[2], 
        delta = r$estimate[3], mu = r$estimate[4]), minimum = -r$minimum, 
        code = r$code, gradient = r$gradient, steps = steps)
    new("fDISTFIT", call = as.call(CALL), model = "Hyperbolic Distribution", 
        data = as.data.frame(x.orig), fit = fit, title = as.character(title), 
        description = .description())
}

nigFit <- 
function (x, alpha = 1, beta = 0, delta = 1, mu = 0, doplot = TRUE, 
    span = "auto", trace = FALSE, title = NULL, description = NULL, 
    ...) 
{
    x.orig = x
    x = as.vector(x)
    CALL = match.call()
    .trace <<- trace
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
        lines(x = span, y = log(y.points), col = "steelblue")
        if (exists("grid")) 
            grid()
    }
    if (is.null(title)) 
        title = "Normal Inverse Gaussian Parameter Estimation"
    if (is.null(description)) 
        description = .description()
    fit = list(estimate = r$estimate, minimum = -r$minimum, code = r$code, 
        gradient = r$gradient, steps = steps)
    new("fDISTFIT", call = as.call(CALL), model = "Normal Inverse Gaussian Distribution", 
        data = as.data.frame(x.orig), fit = fit, title = as.character(title), 
        description = .description())
}

.darchm2Copula <- 
function (u = 0.5, v = u, alpha = NULL, type = 1:22, output = c("vector", 
    "list")) 
{#browser()
    output = match.arg(output)
    type = as.integer(type[1])
    if (is.null(alpha)) {
        alpha = .archmParam(type)$param
    }
    a = alpha
    #if (is.list(u)) {
    #    v = u[[2]]
    #    u = u[[1]]
    #}
    #if (is.matrix(u)) {
    #    v = u[, 1]
    #    u = u[, 2]
    #}
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


.darchm1Copula <- 
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
